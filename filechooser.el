;;; filechooser.el --- An xdg-desktop-portal filechooser -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: rahguzar <rahguzar@mailbox.org>
;; Maintainer: rahguzar <rahguzar@mailbox.org>
;; Created: May 20, 2023
;; Version: 0.3.0
;; Keywords: convenience files tools unix
;; Homepage: https://codeberg.org/rahguzar/filechooser
;; Package-Requires: ((emacs "28.1") (compat "29.1"))
;;
;; This file is part of GNU Emacs.
;;
;;; Commentary:
;; An implementation of xdg-desktop-portal filechooser in Emacs. This allows
;; for choosing files in applications like firefox (with GTK_USE_PORTAL set)
;; using Emacs.
;;
;;; Code:
(require 'compat)
(require 'dbus)
(require 'xdg)
(require 'dired)
(require 'mailcap)
(require 'url-util)

;;; Variables
;;;; Keymaps
(defvar-keymap filechooser-dired-overriding-map
  :doc "Keymap used as `override-global-map' for Dired based file selection."
  "RET" #'filechooser-dired-return
  "C-c C-c" #'exit-recursive-edit
  "C-c C-k" #'filechooser-abort
  "C-c C-s" #'filechooser-dired-selection-mode
  "C-c C-u" #'filechooser-dired-clear-selection
  "C-f" #'filechooser-toggle-filter
  "<remap> <abort-recursive-edit>" #'filechooser-abort)

(defvar-keymap filechooser-mininuffer-map
  "C-f" #'filechooser-toggle-filter
  "<remap> <keyboard-quit>" #'filechooser-abort
  "<remap> <abort-recursive-edit>" #'filechooser-abort
  "<remap> <abort-minibuffers>" #'filechooser-abort)

(defvar-keymap filechooser-multiple-selection-map
  :parent filechooser-mininuffer-map
  "M-TAB" #'filechooser-multiple-continue
  "M-RET" #'filechooser-multiple-finalize-current-selection
  "M-a" #'filechooser-multiple-select-all)

;;;; Custom variables
(defgroup filechooser nil
  "An xdg-desktop-portal filechooser."
  :link '(url-link :tag "Homepage" "https://codeberg.org/rahguzar/filechooser")
  :group 'files)

(defcustom filechooser-save-existing-files 'uniquify
  "Determines behavior when attempting to save an existing file FILENAME.
If it is symbol `yes-or-no-p', `yes-or-no-p' is used to confirm if the
file should be overwritten.  If it is the symbol `y-or-n-p', `y-or-n-p'
is used to prompt.  In both cases if a the answer is negative, the file
selection is started again.  If it is the symbol `uniquify', the
FILENAME is made unique by appedning -N to it where N is a positive
number.  If it is a function, it is called with FILENAME and the return
value is used as the filename."
  :type '(choice
          (const :tag "Uniquify" uniquify)
          (const :tag "Prompt Yes/No" yes-or-no-p)
          (const :tag "Prompt Y/N" y-or-n-p)
          (function :tag "Custom Function")))

(defcustom filechooser-use-popup-frame t
  "Whether to popup a new frame for choosing files.
If it is nil the selected frame is used instead."
  :type 'boolean)

(defcustom filechooser-filters `(("Directories" filechooser-file-directory-p)
                                 ("Elisp files" ,(rx ".el" eos))
                                 ("Not dot files" ,(rx bos (not ?.))))
  "An alist of (NAME FILTER . BOOL).
NAME should describe the filter which can either be a regexp
or else a predicate function which takes a filename as argument.
If BOOL is non-nil filter is active by default otherwise it is inactive."
  :type '(alist :key-type (string :tag "Name")
                :value-type
                (cons :tag "Value:"
                      (choice :tag "Filter" regexp function)
                      (boolean :tag "Default"))))

(defcustom filechooser-choose-file #'filechooser-read-file-name
  "Function used to choose a single file.
It should have the same calling calling convention as
`filechooser-read-file-name' which see for expected behavior."
  :type 'function)

(defcustom filechooser-choose-files #'filechooser-read-file-names
  "Function used to choose multiple files.
It should have the same calling as `filechooser-with-dired' which see for
expected behavior."
  :type 'function)

(defcustom filechooser-choose-directory #'filechooser-save-files
  "Function used to choose a directory for saving files in.
It should have the same calling convention as
`filechooser-save-files' which see for expected behavior."
  :type 'function)

(defcustom filechooser-multiple-selection-key "RET"
  "The key that is used to exit minibuffer to do completion.
I.e. the key that binds the equivalent of `exit-minibuffer' for the completion
UI of choice: usually RET."
  :type (if (> emacs-major-version 27) 'key 'key-sequence))

(defcustom filechooser-dired-marker ?>
  "The character to mark selected files in `filechooser-with-dired'."
  :type 'character)

(defcustom filechooser-dired-selection-debounce 0.2
  "Seconds to wait before updating selection buffer in `filechooser-with-dired'."
  :type 'float)

;;;; Others
(defvar filechooser-current-operation nil
  "When filechooser is active, this variable is set to the command being used.")

;;;; Internal Variables
(defvar filechooser--request-kind 'file)
(defvar-local filechooser--picker-buffer nil)
(defvar filechooser--dired-window nil)
(defvar filechooser--filters nil)
(defvar filechooser--active-filters nil)
(defvar filechooser--multiple-selection nil)
(defvar filechooser--dired-selection nil)
(defvar filechooser--dired-buffers nil)

(defvar-keymap filechooser--special-event-map
  :parent special-event-map
  "<delete-frame>" #'filechooser-abort)

;;; Filters
(defun filechooser--filters-group-fn (cand transform)
  "Group function for selecting filters.
See Info node `(elisp) Programmed Completion' for CAND and TRANSFORM."
  (if transform
      cand
    (if (cdr (alist-get cand filechooser--filters nil nil #'equal))
        "Active"
      "Inactive")))

(defun filechooser-file-directory-p (name)
  "Return non-nil if NAME is an existing directory."
  (file-directory-p (if (and (derived-mode-p 'dired-mode) (not (file-name-absolute-p name)))
                        (expand-file-name name (dired-current-directory))
                      name)))

(defun filechooser--filters (filters)
  "Return FILTERS added to `filechooser-filters'."
  (cl-delete-duplicates (append (copy-tree filechooser-filters) (copy-tree filters))
                        :test #'equal :key #'car))

(defun filechooser--active-filters ()
  "Return active filters."
  (delq nil (mapcar (lambda (flt) (if (cddr flt) (cadr flt)))
                    filechooser--filters)))

(defun filechooser-toggle-filter (arg)
  "Toggle a filter.
With prefix ARG toggle multiple filters using `completing-read-multiple'."
  (interactive "P")
  (cl-flet ((table (str predicate action)
              (if (eq action 'metadata)
                  `(metadata (group-function . filechooser--filters-group-fn))
                (complete-with-action
                 action filechooser--filters str predicate))))
    (when-let ((names
                (ensure-list
                 (let ((minibuffer-completing-file-name nil))
                   (funcall (if arg #'completing-read-multiple #'completing-read)
                            (if arg "Toggle filters: " "Toggle a filter: ")
                            #'table nil t)))))
      (dolist (name names)
        (cl-callf not (cdr (alist-get name filechooser--filters nil nil #'equal))))
      (setq filechooser--active-filters (filechooser--active-filters))
      (if (minibufferp)
          (throw 'continue t)
        (dolist (buf filechooser--dired-buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf (jit-lock-refontify))))))))

(defun filechooser--make-filters (opts)
  "Convert portal filters in OPTS to picker filters."
  (let ((filters (caar (alist-get "filters" opts nil nil #'equal)))
        (current (caar (alist-get "current_filter" opts nil nil #'equal)))
        result)
    (when (and current (not (assoc (car current) filters)))
      (push current filters))
    (dolist (filter filters (nreverse result))
      (let ((rules (cadr filter)))
        (push (cons (car filter)
                    (cons (lambda (name)
                            (let ((case-fold-search nil))
                              (cl-some
                               (lambda (rule)
                                 (pcase (car rule)
                                   (0 (string-match-p (wildcard-to-regexp (cadr rule))
                                                      (file-name-nondirectory name)))
                                   (1 (let ((mime (mailcap-extension-to-mime
                                                   (or (file-name-extension name) ""))))
                                        (and mime (string-match-p
                                                   (wildcard-to-regexp (cadr rule)) mime))))))
                               rules)))
                          (equal (car filter) (car current))))
              result)))))

(defun filechooser--eligible-p (name)
  "Return non-nil when NAME has the requested kind."
  (and name (file-exists-p name)
       (if (eq filechooser--request-kind 'directory)
           (file-directory-p name)
         (not (file-directory-p name)))))

(defun filechooser--filters-predicate (name)
  "Return non-nil if NAME is navigable or matches an active filter."
  (or (filechooser-file-directory-p name)
      (and (not (eq filechooser--request-kind 'directory))
           (let ((case-fold-search nil))
             (or (null filechooser--active-filters)
                 (cl-some (lambda (filter)
                            (cond ((stringp filter) (string-match-p filter name))
                                  ((functionp filter) (funcall filter name))
                                  (t (error "Unknown filter %S" filter))))
                          filechooser--active-filters))))))

;;; Utility definitions
(defmacro filechooser--maybe-with-new-frame (minibuffer &rest body)
  "Excute BODY in a new frame if `filechooser-use-popup-frame' is non-nil.
MINIBUFFER is the value of minibuffer frame paramter."
  (declare (indent 1))
  (let ((framevar (make-symbol "frame")))
    `(let ((minibuffer-completing-file-name ,(eq minibuffer 'only)))
       (if filechooser-use-popup-frame
           (let ((,framevar (make-frame '((name . ,(if (eq minibuffer 'only)
                                                       "filechooser-miniframe"
                                                     "filechooser-frame"))
                                          (minibuffer . ,minibuffer))))
                 (special-event-map filechooser--special-event-map))
             (unwind-protect
                 (catch 'exit
                   (with-selected-frame ,framevar
                     ,@body))
               (delete-frame ,framevar 'force)))
         (catch 'exit
           ,@body)))))

(defun filechooser-abort ()
  "Abort the file selection."
  (interactive)
  (throw 'exit nil))

(defun filechooser--return-value (filenames &rest entries)
  "Construct return value list of FILENAMES and alist ENTRIES."
  `(,(if filenames 0 1)
    ,(if filenames
         `(:array
           (:dict-entry "uris"
                        (:variant
                         (:array ,@(mapcar
                                    (lambda (name)
                                      (concat "file://"
                                              (url-hexify-string
                                               (encode-coding-string (expand-file-name name) 'utf-8)
                                               (append url-unreserved-chars '(?/)))))
                                    (ensure-list filenames)))))
           ,@(mapcar (lambda (cell) `(:dict-entry ,(car cell) (:variant ,(cdr cell))))
                     entries))
       `(:array :signature "{sv}"))))

;;; Minibuffer based selection
(defun filechooser--multiple-group-function (completion transform)
  "Group function for `filechooser--multiple-loop-table'.
See Info node `(elisp) Programmed Completion' for COMPLETION and TRANSFORM."
  (if-let ((display (rassoc completion filechooser--multiple-selection)))
      (if transform
          (abbreviate-file-name (car display))
        "Selected")
    (if transform completion "Select Multiple")))

(defun filechooser--multiple-loop-table (str pred action)
  "Completion table used for calling `completing-read' in a loop.
See Info node `(elisp) Programmed Completion' for STR, PRED and ACTION."
  (pcase action
    ('t (let ((dir (or (file-name-directory str) default-directory)))
          (cl-callf2 mapcar (lambda (sel) (cons (car sel)
                                           (file-relative-name (car sel) dir)))
                     filechooser--multiple-selection)
          (append (mapcar #'cdr filechooser--multiple-selection)
                  (completion-file-name-table str pred t))))
    ('metadata '(metadata
                 (category . file)
                 (group-function . filechooser--multiple-group-function)))
    (_ (completion-file-name-table str pred action))))

(defun filechooser--read-file-name-1 (prompt &optional mustmatch dir default)
  "Read a filename with PROMPT and predicate made from active filters.
MUSTMATCH and DIR are as in `read-file-name'.  DEFAULT is the default filename."
  (catch 'continue
    (minibuffer-with-setup-hook
        (lambda () (use-local-map (make-composed-keymap filechooser-mininuffer-map
                                                   (current-local-map)))
          (when dir (setq default-directory dir)))
      (let ((use-file-dialog nil))
        (read-file-name
         prompt dir default mustmatch nil #'filechooser--filters-predicate)))))

(defun filechooser--handle-exisiting-file (filename &optional dir filters)
  "Handle an existing FILENAME according to `filechooser-save-existing-files'.
DIR is the directory to use if a new file name needs to be choosen and FILTERS
are the filters to use in that case."
  (pcase filechooser-save-existing-files
    ('uniquify
     (let ((n 1)
           (name (file-name-sans-extension filename))
           (ext (file-name-extension filename t)))
       (while (file-exists-p (format "%s-%s%s" name n ext))
         (cl-incf n))
       (format "%s-%s%s" name n ext)))
    ((or 'yes-or-no-p 'y-or-n-p)
     (if (funcall filechooser-save-existing-files
                  (format "File %s exists. Overwrite?" filename))
         filename
       (filechooser--read-file-name "Choose a new file name: "
                                    nil filters dir
                                    (file-relative-name filename dir))))
    (_ (funcall filechooser-save-existing-files filename))))

(defun filechooser--read-file-name (prompt &optional mustmatch filters dir default)
  "Read a filename with PROMPT and predicate made from FILTERS.
MUSTMATCH and DIR are as in `read-file-name'.  DEFAULT is the default filename.
If MULTIPLE is non-nil `completing-read-multiple' is used."
  (let* ((result t)
         (filechooser--filters (filechooser--filters filters))
         (filechooser--active-filters (filechooser--active-filters)))
    (while (or (eq t result)
               (and (eq mustmatch t) result (not (filechooser--eligible-p result))))
      (when (minibufferp nil t)
        (abort-minibuffers))
      (setq result (filechooser--read-file-name-1 prompt mustmatch dir default))
      (when (stringp result)
        (setq result (expand-file-name result dir))))
    (when (and result default (file-directory-p result))
      (setq result (expand-file-name default result)))
    (if (or (null result) mustmatch (not (file-exists-p result)))
        result
      (filechooser--handle-exisiting-file result dir filters))))

(defun filechooser-read-file-name (prompt &optional dir filters mustmatch default)
  "Read a file name.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.
PROMPT is the minibuffer prompt.  MUSTMATCH and DIR are as in `read-file-name'.
FILTERS take the same form as elements of `filechooser-filters'. Only those
files which satisfy one of the active filters from FILTERS or
`filechooser-filters' are presented for completions."
  (filechooser--maybe-with-new-frame only
    (filechooser--read-file-name prompt mustmatch filters dir default)))

(defun filechooser-multiple-continue ()
  "Select current file and exit multiple file selection."
  (interactive)
  (setq this-command 'filechooser-multiple-continue)
  (call-interactively
   (key-binding (kbd filechooser-multiple-selection-key))))

(defun filechooser-multiple-finalize-current-selection ()
  "Exit the file selection with currently selected candidates."
  (interactive)
  (throw 'done nil))

(defun filechooser-multiple-select-all ()
  "Select all of current completion candidates."
  (interactive)
  (setq this-command 'filechooser-multiple-continue)
  (let* ((str (minibuffer-contents))
         (dir (file-name-directory str))
         (cands (completion-all-completions
                 str minibuffer-completion-table nil (length str))))
    (when (cdr (last cands))
      (setf (cdr (last cands)) nil))
    ;; Handle `completion-ignored-extensions'.
    (cl-callf completion-pcm--filename-try-filter cands)
    (setq filechooser--multiple-selection
          (delete-dups
           (mapcar (lambda (cand) (cons (expand-file-name cand dir) t))
                   (cl-remove-if-not (lambda (cand) (filechooser--eligible-p (expand-file-name cand dir))) cands))))
    (throw 'done dir)))

(defun filechooser--multiple-read-file-name (prompt &optional dir)
  "Read a filename with PROMPT and starting from DIR.
MAP contains additional key bindigs."
  (let ((result t))
    (while (eq t result)
      (when (minibufferp nil t)
        (abort-minibuffers))
      (setq result
            (catch 'continue
              (minibuffer-with-setup-hook
                  (lambda () (use-local-map (make-composed-keymap
                                        filechooser-multiple-selection-map
                                        (current-local-map))))
                (completing-read prompt #'filechooser--multiple-loop-table
                                 #'filechooser--filters-predicate t
                                 (abbreviate-file-name dir)
                                 'file-name-history)))))
    result))

(defun filechooser-read-file-names (prompt &optional dir filters)
  "Read multiple file names using `completing-read-multiple'.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.
PROMPT is the minibuffer prompt. DIR is the directory where selection starts.
FILTERS take the same form as elements of `filechooser-filters'. Only those
files which satisfy one of the active filters from FILTERS or
`filechooser-filters' are presented for completions."
  (setq dir (file-name-as-directory
             (expand-file-name (or dir default-directory))))
  (let* ((filechooser--filters (filechooser--filters filters))
         (filechooser--active-filters (filechooser--active-filters))
         selected filechooser--multiple-selection)
    (filechooser--maybe-with-new-frame only
      (while (setq dir
                   (catch 'done
                     (setq selected (expand-file-name
                                     (filechooser--multiple-read-file-name
                                      prompt dir)))
                     (when (filechooser--eligible-p selected)
                       (cl-callf not
                           (alist-get selected filechooser--multiple-selection
                                      nil t #'equal)))
                     (when (eq this-command 'filechooser-multiple-continue)
                       (expand-file-name (file-name-directory selected))))))
      (nreverse (mapcar #'car filechooser--multiple-selection)))))

(defun filechooser-save-files (prompt &optional dir files)
  "Read a directory name to save FILES in it.
If `filechooser-use-popup-frame' is non-nil a new minibuffer only popup frame
is used, othewise the selected frame is used.  PROMPT and DIR are as in
`read-directory-name'."
  (filechooser--maybe-with-new-frame only
    (when-let ((save-dir (read-directory-name prompt dir)))
      (let (names)
        (make-directory save-dir t)
        (catch 'abort
          (dolist (file files)
            (let ((target (expand-file-name file save-dir)))
              (when (or (file-exists-p target) (member target names))
                (if (eq filechooser-save-existing-files 'uniquify)
                    (let ((n 1) (base (file-name-sans-extension target))
                          (ext (or (file-name-extension target t) "")))
                      (while (or (file-exists-p (format "%s-%d%s" base n ext))
                                 (member (format "%s-%d%s" base n ext) names))
                        (cl-incf n))
                      (setq target (format "%s-%d%s" base n ext)))
                  (setq target (filechooser--handle-exisiting-file target save-dir))
                  (unless target (throw 'abort nil))
                  (setq target (expand-file-name target save-dir))
                  (when (member target names) (throw 'abort nil))))
              (push target names)))
          (nreverse names))))))

;;; Dired based selection
(define-minor-mode filechooser-dired-selection-mode
  "Minor mode that is activated during Dired based file selection.
It can be disabled to temporarily pause the selection and do other things
without exiting file selection."
  :global t
  :keymap nil
  (if filechooser-dired-selection-mode
      (progn
        (put 'filechooser-dired-marker 'filechooser--original dired-marker-char)
        (setq dired-marker-char filechooser-dired-marker))
    (setq dired-marker-char
          (or (get 'filechooser-dired-marker 'filechooser--original)
              dired-marker-char))))

(declare-function filechooser--adjust-selection-buffer nil)
(declare-function filechooser--selection-files nil)
(declare-function filechooser--reset-selection nil)
(declare-function filechooser--cancel-selection-timer nil)
(declare-function filechooser--process-changed-marks nil)
(declare-function filechooser-dired-clear-selection nil)

(let ((selection (make-hash-table :test #'equal))
      timer)

  (defun filechooser--reset-selection ()
    (clrhash selection))

  (defun filechooser--selection-files ()
    (hash-table-keys selection))

  (defun filechooser--cancel-selection-timer ()
    (when timer (cancel-timer timer) (setq timer nil)))

  (defun filechooser--adjust-selection-buffer ()
    (filechooser--cancel-selection-timer)
    (when (buffer-live-p (cdr filechooser--dired-selection))
      (with-current-buffer (cdr filechooser--dired-selection)
        (setf (cdr dired-directory) (hash-table-keys selection))
        (revert-buffer))))

  (defun filechooser-dired-clear-selection (&optional beg end)
    "Remove files from BEG to END from selection."
    (interactive)
    (with-current-buffer (cdr filechooser--dired-selection)
      (let ((dired-marker-char (get 'filechooser-dired-marker 'filechooser--original))
            (filechooser-dired-selection-mode nil))
        (if (eq t end)
            (dired-mark beg t)
          (dired-mark-files-in-region (or beg (point-min)) (or end (point-max))))
        (let ((files (delq nil (dired-map-over-marks
                                (dired-get-filename nil t) nil))))
          (dolist (buf filechooser--dired-buffers)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (save-excursion
                  (dolist (file files)
                    (when (dired-goto-file file)
                      (dired-unmark nil)))))))
          (dolist (file files)
            (remhash file selection)))))
    (filechooser--adjust-selection-buffer))

  (defun filechooser--process-changed-marks (beg end _length)
    "Deal with change in mark from BEG to END."
    (when (and filechooser-dired-selection-mode
               filechooser--picker-buffer
               (derived-mode-p 'dired-mode)
               (eq (1+ beg) end))
      (save-excursion
        (goto-char beg)
        (cond
         ((or (not (filechooser--eligible-p (dired-get-filename nil t)))
              (invisible-p (1- (pos-eol)))
              (not (filechooser--filters-predicate
                    (dired-get-filename 'no-dir t))))
          (dired-unmark nil))
         ((and (re-search-forward dired-re-mark end t)
               (eq (preceding-char) dired-marker-char))
          (puthash (dired-get-filename nil t) t selection))
         ((and (re-search-forward dired-re-maybe-mark (1+ end) t))
          (remhash (dired-get-filename nil t) selection)))
        (unless timer
          (setq timer (run-with-timer
                       filechooser-dired-selection-debounce nil
                       #'filechooser--adjust-selection-buffer)))))))

(defun filechooser-dired-deselect (arg &optional interactive)
  "Deselect the file at point in selection buffer.
See `dired-mark' for ARG and INTERACTIVE."
  (interactive (list current-prefix-arg t))
  (filechooser-dired-clear-selection arg interactive))

(defun filechooser--dired-setup-buffer (&optional _)
  "Setup the current buffer for file selection."
  (when (and (eq (selected-window) filechooser--dired-window)
             (derived-mode-p 'dired-mode)
             (not (memq (current-buffer)
                        `(,(cdr filechooser--dired-selection)
                          ,@filechooser--dired-buffers))))
    (setq-local filechooser--picker-buffer t)
    (add-hook 'jit-lock-functions #'filechooser--dired-jit-filter 95 t)
    (jit-lock-mode t)
    (add-to-invisibility-spec 'filechooser-filter)
    (cl-pushnew (current-buffer) filechooser--dired-buffers)))

(defun filechooser-dired-return ()
  "Navigate directories or accept the file at point."
  (interactive)
  (let ((name (dired-get-filename nil t)))
    (cond ((and name (file-directory-p name))
           (dired-find-file)
           (filechooser--dired-setup-buffer))
          ((filechooser--eligible-p name) (exit-recursive-edit)))))

(defun filechooser-dired (&optional dir filters)
  "Select some files using Dired.
Running this command pops a Dired for directory DIR, and enters a recursive
editing session.  FILTERS are in the format of `filechooser-filters'."
  (unless (and filechooser--dired-selection
               (file-directory-p (car filechooser--dired-selection)))
    (setq filechooser--dired-selection (list (make-temp-file "filechooser-selection-" t))))
  (let* ((overriding-map `((filechooser--picker-buffer . ,filechooser-dired-overriding-map)))
         (filechooser--filters (filechooser--filters filters))
         (filechooser--active-filters (filechooser--active-filters))
         (dired-kill-when-opening-new-dired-buffer nil)
         (dired-mode-hook (cons #'filechooser--dired-setup-buffer dired-mode-hook))
         (post-command-hook (cons #'filechooser--dired-setup-buffer post-command-hook))
         filechooser--dired-window
         filechooser--dired-buffers)
    (save-window-excursion
      (unwind-protect
          (progn
            (pop-to-buffer
             (setcdr filechooser--dired-selection
                     (dired-noselect (list (car filechooser--dired-selection))))
             '(display-buffer-in-side-window
               (side . left) (window-width . 0.3)))
            (filechooser-dired-selection-mode)
            (filechooser-dired-clear-selection)
            (redisplay)
            (with-current-buffer (cdr filechooser--dired-selection)
              (setq mode-line-format " Selected files")
              (dired-hide-details-mode)
              (add-hook 'jit-lock-functions #'filechooser--dired-jit-abbreviate 95 t)
              (jit-lock-mode t)
              (use-local-map
               (define-keymap :parent (current-local-map)
                 "<remap> <dired-unmark>" #'filechooser-dired-deselect
                 "<remap> <dired-unmark-all-marks>" #'filechooser-dired-clear-selection)))
            (push overriding-map emulation-mode-map-alists)
            (add-hook 'window-buffer-change-functions #'filechooser--dired-setup-buffer)
            (add-hook 'after-change-functions #'filechooser--process-changed-marks)
             (other-window 1)
             (setq filechooser--dired-window (selected-window))
             (dired (or dir default-directory))
            (filechooser--dired-setup-buffer nil)
            (unless (recursive-edit)
              (let* ((selected (cl-remove-if-not #'filechooser--eligible-p
                                                  (filechooser--selection-files)))
                     (at-point (and (derived-mode-p 'dired-mode)
                                    (dired-get-filename nil t)))
                     (fallback (if (and (eq filechooser--request-kind 'directory)
                                        (not (filechooser--eligible-p at-point)))
                                   default-directory at-point)))
                (or selected (and (filechooser--eligible-p fallback) (list fallback))))))
        (cl-callf2 delq overriding-map emulation-mode-map-alists)
        (filechooser--cancel-selection-timer)
        (when (buffer-live-p (cdr filechooser--dired-selection))
          (filechooser--adjust-selection-buffer)
          (filechooser-dired-clear-selection))
        (filechooser--cancel-selection-timer)
        (filechooser--reset-selection)
        (filechooser-dired-selection-mode -1)
        (remove-hook 'window-buffer-change-functions #'filechooser--dired-setup-buffer)
        (remove-hook 'after-change-functions #'filechooser--process-changed-marks)
        (kill-buffer (cdr filechooser--dired-selection))
        (setcdr filechooser--dired-selection nil)
        (dolist (buf filechooser--dired-buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq-local filechooser--picker-buffer nil)
              (let ((inhibit-read-only t))
                (let ((pos (point-min)))
                  (while (< pos (point-max))
                    (let ((next (next-single-property-change pos 'invisible nil (point-max))))
                      (when-let ((value (get-text-property pos 'invisible)))
                        (put-text-property pos next 'invisible
                                           (if (listp value) (delq 'filechooser-filter (copy-sequence value))
                                             (unless (eq value 'filechooser-filter) value))))
                      (setq pos next)))))
              (jit-lock-unregister #'filechooser--dired-jit-filter)
              (remove-from-invisibility-spec 'filechooser-filter))))))))

(defun filechooser--dired-jit-filter (beg end)
  "Hide files that don't match current filters from BEG to END."
  (setq end (progn (goto-char end) (forward-line 1) (pos-eol)))
  (setq beg (progn (goto-char beg) (forward-line -1) (goto-char (pos-bol))))
  (while (< (point) end)
    (when-let ((name (dired-get-filename 'no-dir t)))
      (alter-text-property (1- (point)) (pos-eol) 'invisible
                           (lambda (val)
                             (setq val (ensure-list val))
                             (if (filechooser--filters-predicate name)
                                 (cl-callf2 delq 'filechooser-filter val)
                               (cl-pushnew 'filechooser-filter val)))))
    (forward-line))
  `(jit-lock-bounds ,beg . ,end))

(defun filechooser--dired-jit-abbreviate (beg end)
  "Ellipesize filenames from BEG to END."
  (setq end (progn (goto-char end) (pos-eol)))
  (setq beg (progn (goto-char beg) (goto-char (pos-eol))))
  (while (< (point) end)
    (when-let ((file (dired-get-filename nil t))
               (name (file-name-directory file))
               (name-end (+ (dired-move-to-filename) (length name))))
      (put-text-property (point) name-end 'display ".../")
      (put-text-property (point) name-end 'help-echo file))
    (forward-line))
  `(jit-lock-bounds ,beg . ,end))

(defun filechooser-with-dired (_prompt &optional dir filters)
  "Select some files using Dired.
If `filechooser-use-popup-frame' is non-nil a new frame is used for selection,
otherwise selected frame is used. DIR is the directory for initial Dired
buffer.  FILTERS are used to restrict selection to a subset of files."
  (filechooser--maybe-with-new-frame t (filechooser-dired dir filters)))

;;; Method handlers
(defun filechooser--option-path (opts name)
  "Decode byte-array option NAME from OPTS."
  (when-let ((bytes (caar (alist-get name opts nil nil #'equal))))
    (dbus-byte-array-to-string (if (equal (car (last bytes)) 0) (butlast bytes) bytes))))

(defun filechooser--prompt (action app-id title)
  "Format ACTION using APP-ID or the application suffix of TITLE."
  (let* ((safe-id (and (stringp app-id)
                       (string-match-p "\\`[[:alnum:]_][[:alnum:]_.-]*\\'" app-id)
                       app-id))
         (desktop-name
          (and safe-id
               (condition-case nil
                   (let ((file (locate-file (concat safe-id ".desktop")
                                            (mapcar (lambda (dir) (expand-file-name "applications" dir))
                                                    (cons (xdg-data-home) (xdg-data-dirs))))))
                     (when file (gethash "Name" (xdg-desktop-read-file file))))
                 (error nil))))
         (desktop-name (and (stringp desktop-name) (string-trim desktop-name)))
         (suffix (and (stringp title)
                      (string-match ".*[[:space:]][-–—][[:space:]]+\\([^–—]+\\)\\'" title)
                      (string-trim (match-string 1 title))))
         (app (or (and desktop-name (not (string-empty-p desktop-name)) desktop-name)
                  (and suffix (not (string-empty-p suffix)) suffix)
                  safe-id)))
    (concat action (if app (format " [%s]" app) "") ": ")))

(defun filechooser-handle-open-file (_handle app-id _parent title &rest opts)
  "Handle OpenFile request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (let* ((filters (filechooser--make-filters opts))
         (multiple (caar (alist-get "multiple" opts nil nil #'equal)))
         (filechooser--request-kind (if (caar (alist-get "directory" opts nil nil #'equal))
                                       'directory 'file))
         (dir (filechooser--option-path opts "current_folder"))
         (dir (and dir (file-name-as-directory dir)))
         (chooser (if multiple filechooser-choose-files filechooser-choose-file))
         (prompt (filechooser--prompt (if (eq filechooser--request-kind 'directory)
                                          "Select folder" "Open file")
                                      app-id title))
         (result (progn
                   (setq filechooser-current-operation chooser)
                   (if multiple (funcall chooser prompt dir filters)
                     (funcall chooser prompt dir filters t))))
         (names (ensure-list result)))
    (cond ((null result) (filechooser--return-value nil))
          ((or (not (listp names))
               (and (not multiple) (/= (length names) 1))
               (not (cl-every (lambda (name) (and (stringp name) (filechooser--eligible-p name))) names)))
           '(2 (:array :signature "{sv}")))
          (t (filechooser--return-value names)))))

(defun filechooser-handle-save-file (_handle app-id _parent title &rest opts)
  "Handle SaveFile request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (setq filechooser-current-operation filechooser-choose-file)
  (let* ((current-file (filechooser--option-path opts "current_file"))
         (folder (or (filechooser--option-path opts "current_folder")
                     (and current-file (file-name-directory current-file))))
         (name (or (caar (alist-get "current_name" opts nil nil #'equal))
                   (and current-file (file-name-nondirectory current-file)))))
    (filechooser--return-value
     (funcall filechooser-choose-file (filechooser--prompt "Save file" app-id title)
              (and folder (file-name-as-directory folder))
              (filechooser--make-filters opts) nil name))))

(defun filechooser-handle-save-files (_handle app-id _parent title &rest opts)
  "Handle SaveFiles request with prompt TITLE and options OPTS."
  (setq opts (or (plist-get opts :array) (car opts)))
  (setq filechooser-current-operation filechooser-choose-directory)
  (filechooser--return-value
   (funcall
    filechooser-choose-directory
    (filechooser--prompt "Save files" app-id title)
    (dbus-byte-array-to-string
     (butlast (caar (alist-get "current_folder" opts nil nil #'equal))))
    (mapcar (lambda (file) (dbus-byte-array-to-string (butlast file)))
            (caar (alist-get "files" opts nil nil #'equal))))))

;;; Starting Service
;;;###autoload
(defun filechooser-start ()
  "Start the filechooser.

It works by using d-bus and xdg-desktop-portal and implements
`org.freedesktop.impl.portal.FileChooser'"
  (interactive)
  (cl-flet ((register (method handler)
              (dbus-register-method
               :session "org.gnu.Emacs.FileChooser"
               "/org/freedesktop/portal/desktop"
               "org.freedesktop.impl.portal.FileChooser"
               method (lambda (&rest args)
                        (condition-case err
                            (apply handler args)
                          (quit (filechooser--return-value nil))
                          (error
                           (message "Filechooser %s failed: %s" method
                                    (error-message-string err))
                           '(2 (:array :signature "{sv}"))))) t)))
    (register "OpenFile" 'filechooser-handle-open-file)
    (register "SaveFile" 'filechooser-handle-save-file)
    (register "SaveFiles" 'filechooser-handle-save-files)
    (dbus-register-method :session "org.gnu.Emacs.FileChooser"
                          nil "org.freedesktop.DBus.Properties"
                          "GetAll" (lambda (&rest _args) :ignore) t)
    (dbus-register-service :session "org.gnu.Emacs.FileChooser")))

(provide 'filechooser)
;;; filechooser.el ends here
