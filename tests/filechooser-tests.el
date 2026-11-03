;;; filechooser-tests.el --- Picker regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'filechooser)
(require 'url-util)

(defmacro fc-test-with-files (&rest body)
  `(let* ((root (make-temp-file "filechooser-test-" t))
          (child (expand-file-name "child" root))
          (a (expand-file-name "a.txt" root))
          (png (expand-file-name "b.png" root))
          (nested (expand-file-name "nested.txt" child))
          (filechooser-use-popup-frame nil)
          (filechooser--dired-selection nil))
     (unwind-protect
         (progn (make-directory child)
                (dolist (f (list a png nested)) (write-region "x" nil f nil 'silent))
                ,@body)
       (dolist (buf (buffer-list))
         (when (with-current-buffer buf (and (derived-mode-p 'dired-mode)
                                              (string-prefix-p root default-directory)))
           (kill-buffer buf)))
       (delete-directory root t))))

(defun fc-test-dired (root action &optional filters)
  (cl-letf (((symbol-function 'recursive-edit) action))
    (filechooser-dired root filters)))

(defun fc-test-opts (kind pattern)
  `(("filters" ((("Images" ((,kind ,pattern))))))
    ("current_filter" (("Images" ((,kind ,pattern)))))))
(defun fc-test-bytes (path) (string-to-list (concat path "\0")))
(defun fc-test-uris (response)
  (cdr (cadr (nth 2 (cadr (cadr response))))))

(ert-deftest filechooser-dired-ret-accepts-file ()
  (fc-test-with-files
   (let (visited)
     (should (equal (list a)
                    (fc-test-dired root (lambda ()
                      (dired-goto-file a)
                      (cl-letf (((symbol-function 'exit-recursive-edit) (lambda () (throw 'accepted nil))))
                        (catch 'accepted (call-interactively (key-binding (kbd "RET")))))
                      (setq visited buffer-file-name) nil))))
     (should-not visited))))
(ert-deftest filechooser-dired-confirm-point ()
  (fc-test-with-files
   (should (equal (list a) (fc-test-dired root (lambda () (dired-goto-file a) nil))))))
(ert-deftest filechooser-dired-cross-directory-marks ()
  (fc-test-with-files
   (let ((dired-kill-when-opening-new-dired-buffer t))
     (should (equal (sort (list a nested) #'string<)
                    (sort (fc-test-dired root (lambda ()
                      (dired-goto-file a) (dired-mark 1)
                      (dired-goto-file child) (dired-find-file)
                      (filechooser--dired-setup-buffer nil)
                      (dired-goto-file nested) (dired-mark 1) nil)) #'string<))))))
(ert-deftest filechooser-dired-cancel-after-navigation ()
  (fc-test-with-files
   (let ((dired-kill-when-opening-new-dired-buffer t))
     (should (equal 'cancelled
                    (catch 'exit (fc-test-dired root (lambda ()
                      (dired-goto-file child) (dired-find-file)
                      (filechooser--dired-setup-buffer nil) (throw 'exit 'cancelled)))))))))
(ert-deftest filechooser-dired-header-does-not-select-nil ()
  (fc-test-with-files
   (should-not (fc-test-dired root (lambda () (goto-char (point-min)) nil)))))
(ert-deftest filechooser-filter-navigation-and-case ()
  (fc-test-with-files
   (let* ((filechooser--filters (filechooser--filters (filechooser--make-filters (fc-test-opts 0 "*.png"))))
          (filechooser--active-filters (filechooser--active-filters)))
     (with-temp-buffer
       (dired-mode root)
       (should (filechooser--filters-predicate child))
       (should (filechooser--filters-predicate png))
       (should-not (filechooser--filters-predicate a))
       (should-not (filechooser--filters-predicate (expand-file-name "B.PNG" root)))))))
(ert-deftest filechooser-mime-filter ()
  (fc-test-with-files
   (let* ((filechooser--filters (filechooser--filters (filechooser--make-filters (fc-test-opts 1 "image/png"))))
          (filechooser--active-filters (filechooser--active-filters)))
     (should (filechooser--filters-predicate png))
     (should-not (filechooser--filters-predicate a)))))
(ert-deftest filechooser-folder-request-rejects-files ()
  (fc-test-with-files
   (dolist (multiple '(nil t))
     (let ((filechooser-choose-file (lambda (&rest _) a))
           (filechooser-choose-files (lambda (&rest _) (list child a))))
       (should (= 2 (car (filechooser-handle-open-file nil nil nil "Open"
                           `(("directory" (t)) ("multiple" (,multiple)))))))))))
(ert-deftest filechooser-open-current-folder ()
  (fc-test-with-files
   (let* (seen (filechooser-choose-file (lambda (_ dir &rest _) (setq seen dir) a)))
     (filechooser-handle-open-file nil nil nil "Open" `(("current_folder" (,(fc-test-bytes child)))))
     (should (equal seen (file-name-as-directory child))))))
(ert-deftest filechooser-save-current-file ()
  (fc-test-with-files
   (let* (seen (filechooser-choose-file (lambda (_ dir _filters _must default) (setq seen (expand-file-name default dir)) a)))
     (filechooser-handle-save-file nil nil nil "Save" `(("current_file" (,(fc-test-bytes a)))))
     (should (equal seen a)))))
(ert-deftest filechooser-uri-escaping ()
  (fc-test-with-files
   (let* ((name (expand-file-name "sp ace#?%ü.txt" root))
          (response (filechooser--return-value name))
          (uri (car (fc-test-uris response))))
     (should (equal uri (concat "file://" root "/sp%20ace%23%3F%25%C3%BC.txt")))
     (should (equal (decode-coding-string (url-unhex-string (substring uri 7)) 'utf-8) name)))))
(ert-deftest filechooser-save-files-order-and-duplicates ()
  (fc-test-with-files
   (let ((filechooser-save-existing-files 'uniquify))
     (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) root)))
       (should (equal (list (expand-file-name "a-1.txt" root)
                            (expand-file-name "new.txt" root)
                            (expand-file-name "new-1.txt" root))
                      (filechooser-save-files "Save" root '("a.txt" "new.txt" "new.txt"))))))))
(ert-deftest filechooser-save-files-custom-collision-and-cancel ()
  (fc-test-with-files
   (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) root)))
     (let ((filechooser-save-existing-files (lambda (_name) "renamed.txt")))
       (should (equal (list (expand-file-name "renamed.txt" root))
                      (filechooser-save-files "Save" root '("a.txt")))))
     (let ((filechooser-save-existing-files (lambda (_name) nil)))
       (should-not (filechooser-save-files "Save" root '("new.txt" "a.txt")))))))

(ert-deftest filechooser-dired-directory-context ()
  (fc-test-with-files
   (let ((filechooser--request-kind 'directory))
     (should (equal (list child)
                    (fc-test-dired root (lambda ()
                      (dired-goto-file a) (dired-mark 1)
                      (dired-goto-file child) (dired-mark 1) nil))))
     (should (equal (list (file-name-as-directory root))
                    (fc-test-dired root (lambda () (goto-char (point-min)) nil)))))))

(ert-deftest filechooser-dired-dead-tracked-buffer ()
  (fc-test-with-files
   (should (equal (list nested)
                  (fc-test-dired root (lambda ()
                    (let ((old (current-buffer)))
                      (dired-goto-file child) (dired-find-file)
                      (filechooser--dired-setup-buffer nil)
                      (kill-buffer old)
                      (dired-goto-file nested) (dired-mark 1) nil)))))))

(ert-deftest filechooser-dired-navigation-installs-bindings-immediately ()
  (fc-test-with-files
   (should (equal (list nested)
                  (fc-test-dired root (lambda ()
                    (dired-goto-file child)
                    (dired-find-file)
                    (should (eq (key-binding (kbd "RET")) #'filechooser-dired-return))
                    (dired-goto-file nested)
                    (dired-mark 1)
                    nil))))))

(ert-deftest filechooser-dired-ret-reuses-existing-directory-buffer ()
  (fc-test-with-files
   (dired-noselect child)
   (should (equal (list nested)
                  (fc-test-dired root (lambda ()
                    (dired-goto-file child)
                    (call-interactively (key-binding (kbd "RET")))
                    (should (eq (key-binding (kbd "RET")) #'filechooser-dired-return))
                    (dired-goto-file nested)
                    (dired-mark 1)
                    nil))))))

(ert-deftest filechooser-dired-does-not-claim-other-windows ()
  (fc-test-with-files
   (should-not
    (fc-test-dired root (lambda ()
      (save-selected-window
        (select-window (split-window-below))
        (dired child)
        (filechooser--dired-setup-buffer)
        (should-not filechooser--picker-buffer)
        (should (eq (key-binding (kbd "RET")) #'dired-find-file)))
      (goto-char (point-min)) nil)))))

(ert-deftest filechooser-directory-only-completion-hides-files ()
  (fc-test-with-files
   (let ((filechooser--request-kind 'directory))
     (should (filechooser--filters-predicate child))
     (should-not (filechooser--filters-predicate a)))))

(ert-deftest filechooser-dired-up-reuses-existing-parent-buffer ()
  (fc-test-with-files
   (dired-noselect root)
   (should (equal (list a)
                  (fc-test-dired child (lambda ()
                    (dired-up-directory)
                    (run-hooks 'post-command-hook)
                    (should (eq (key-binding (kbd "RET")) #'filechooser-dired-return))
                    (dired-goto-file a) (dired-mark 1) nil))))))

(ert-deftest filechooser-filters-are-copied ()
  (let* ((filechooser-filters '(("Test" "x" . t)))
         (filechooser--filters (filechooser--filters nil)))
    (setcdr (cdr (car filechooser--filters)) nil)
    (should (cddar filechooser-filters))))

(ert-deftest filechooser-dired-filter-does-not-select-directory ()
  (fc-test-with-files
   (should (equal (list png)
                  (fc-test-dired root (lambda ()
                    (dired-goto-file child) (dired-mark 1)
                    (dired-goto-file png) (dired-mark 1) nil)
                    (filechooser--make-filters (fc-test-opts 0 "*.png")))))))

(ert-deftest filechooser-open-directory-built-in-dired ()
  (fc-test-with-files
   (dolist (multiple '(nil t))
     (let* ((filechooser-choose-file
             (lambda (_prompt dir filters &rest _)
               (car (fc-test-dired dir (lambda ()
                 (dired-goto-file a) (dired-mark 1)
                 (dired-goto-file child) (dired-mark 1) nil) filters))))
            (filechooser-choose-files
             (lambda (_prompt dir filters)
               (fc-test-dired dir (lambda ()
                 (dired-goto-file a) (dired-mark 1)
                 (dired-goto-file child) (dired-mark 1) nil) filters)))
            (response (filechooser-handle-open-file nil nil nil "Open"
                        `(("directory" (t)) ("multiple" (,multiple))
                          ("current_folder" (,(fc-test-bytes root)))))))
       (should (= 0 (car response)))
       (should (equal (list (concat "file://" child)) (fc-test-uris response)))))))

(ert-deftest filechooser-registered-handler-contains-errors ()
  (let ((filechooser-use-popup-frame nil) handlers)
    (cl-letf (((symbol-function 'dbus-register-method)
               (lambda (_bus _service _path _interface method handler &rest _)
                 (push (cons method handler) handlers)))
              ((symbol-function 'dbus-register-service) #'ignore)
              ((symbol-function 'read-directory-name) (lambda (&rest _) "/unwritable"))
              ((symbol-function 'make-directory) (lambda (&rest _) (signal 'file-error '("denied")))))
      (filechooser-start)
      (let ((handler (cdr (assoc "SaveFiles" handlers))))
        (should (equal '(2 (:array :signature "{sv}"))
                       (funcall handler nil nil nil "Save"
                                `(("current_folder" (,(fc-test-bytes "/tmp")))
                                  ("files" (nil))))))
        (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) (signal 'quit nil))))
          (should (equal '(1 (:array :signature "{sv}"))
                         (funcall handler nil nil nil "Save"
                                  `(("current_folder" (,(fc-test-bytes "/tmp")))
                                    ("files" (nil)))))))))))

(ert-deftest filechooser-read-relative-against-requested-directory ()
  (fc-test-with-files
   (let ((default-directory "/"))
     (dolist (case `((file "a.txt" ,a) (directory "child" ,child)))
       (let ((filechooser--request-kind (car case))
             (answers (list (cadr case) nil)))
         (cl-letf (((symbol-function 'filechooser--read-file-name-1)
                    (lambda (&rest _) (pop answers))))
           (should (equal (caddr case)
                          (filechooser--read-file-name "Open" t nil root))))
         (setq answers (list (cadr case) nil))
         (cl-letf (((symbol-function 'filechooser--read-file-name-1)
                    (lambda (&rest _) (pop answers))))
           (should (equal (list (concat "file://" (caddr case)))
                          (fc-test-uris
                           (filechooser-handle-open-file
                            nil nil nil "Open"
                            `(("directory" (,(eq (car case) 'directory)))
                              ("current_folder" (,(fc-test-bytes root))))))))))))))

(ert-deftest filechooser-read-preserves-suggested-name ()
  (fc-test-with-files
   (cl-letf (((symbol-function 'filechooser--read-file-name-1)
              (lambda (&rest _) "new.txt")))
     (should (equal (expand-file-name "new.txt" root)
                    (filechooser--read-file-name "Save" nil nil root "new.txt"))))))

(ert-deftest filechooser-read-confirm-allows-new-file ()
  (fc-test-with-files
   (dolist (mustmatch '(confirm confirm-after-completion))
     (let ((answers (list "new.txt" nil)))
       (cl-letf (((symbol-function 'filechooser--read-file-name-1)
                  (lambda (&rest _) (pop answers))))
         (should (equal (expand-file-name "new.txt" root)
                        (filechooser--read-file-name "Open" mustmatch nil root))))))))

(defun fc-test-selection-timers ()
  (cl-remove-if-not (lambda (timer)
                      (eq (timer--function timer) #'filechooser--adjust-selection-buffer))
                    timer-list))

(ert-deftest filechooser-dired-clear-cancels-pending-timer ()
  (fc-test-with-files
   (let ((filechooser-dired-selection-debounce 3600))
     (unwind-protect
         (dotimes (_ 2)
           (fc-test-dired root (lambda ()
             (dired-goto-file a) (dired-mark 1)
             (should (fc-test-selection-timers))
             (filechooser-dired-clear-selection)
             (should-not (fc-test-selection-timers)) nil))
           (should-not (fc-test-selection-timers)))
       (mapc #'cancel-timer (fc-test-selection-timers))))))

(ert-deftest filechooser-dired-ignores-unrelated-marks ()
  (fc-test-with-files
   (let ((other (dired-noselect child)))
     (should-not
      (fc-test-dired root (lambda ()
        (with-current-buffer other (dired-goto-file nested) (dired-mark 1))
        (goto-char (point-min)) nil))))))
