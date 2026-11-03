;;; portal-driver.el --- Isolated interactive portal verification -*- lexical-binding: t; -*-

(require 'filechooser)
(require 'server)
(require 'json)

(setq server-name (getenv "FILECHOOSER_TEST_SERVER")
      filechooser-use-popup-frame nil
      filechooser-choose-files #'filechooser-with-dired
      filechooser-filters nil
      filechooser-save-existing-files 'yes-or-no-p
      dired-kill-when-opening-new-dired-buffer t
      dired-listing-switches "-alh --group-directories-first"
      default-directory (file-name-as-directory (getenv "FILECHOOSER_TEST_ROOT")))
(server-start)

(defvar filechooser-test-steps nil)
(defvar filechooser-test-observations nil)

(defun filechooser-test-step ()
  (interactive)
  (funcall (pop filechooser-test-steps)))

(global-set-key (kbd "<f12>") #'filechooser-test-step)

(defun filechooser-test-goto (name &optional check-visible)
  (let ((path (expand-file-name name (getenv "FILECHOOSER_TEST_ROOT"))))
    (lambda ()
      (unless (dired-goto-file path)
        (error "Fixture missing from Dired: %s" path))
      (when check-visible
        (jit-lock-fontify-now (line-beginning-position) (line-end-position))
        (when (invisible-p (1- (line-end-position)))
          (error "Navigation directory hidden by filter: %s" path))))))

(defun filechooser-test-insert (name)
  (lambda ()
    (unless (minibufferp)
      (error "Expected an active filename minibuffer"))
    (delete-minibuffer-contents)
    (insert (expand-file-name name (getenv "FILECHOOSER_TEST_ROOT")))))

(defun filechooser-test-request (original &rest args)
  (let* ((title (nth 3 args))
         (plan
          (pcase title
            ((or "nested" "filtered-navigation")
             (list "<f12> RET <f12> m C-c C-c"
                   (filechooser-test-goto "child" (equal title "filtered-navigation"))
                   (filechooser-test-goto "child/nested.png")))
            ("return" (list "<f12> RET C-c C-c" (filechooser-test-goto "plain.txt")))
            ("confirm" (list "<f12> C-c C-c" (filechooser-test-goto "plain.txt")))
            ("folder-invalid" (list "<f12> RET <f12> RET"
                                    (filechooser-test-insert "plain.txt")
                                    (filechooser-test-insert "child/")))
            ("folder-single" (list "<f12> RET" (filechooser-test-insert "child/")))
            ("folder-multiple"
             (list "<f12> m <f12> m C-c C-c"
                   (filechooser-test-goto "plain.txt") (filechooser-test-goto "child")))
            ("uri" (list "<f12> RET" (filechooser-test-insert "sp ace#?%ü.txt")))
            ("savefiles" (list "<f12> RET" (filechooser-test-insert "child/")))
            ("failure" (list "<f12> RET" (filechooser-test-insert "plain.txt/child/")))
            ("cancel" (list "<f12> RET C-c C-k" (filechooser-test-goto "child")))
            (_ (error "Unknown verification request: %s" title))))
         (filechooser-test-steps (cdr plan))
         (unread-command-events (listify-key-sequence (kbd (car plan)))))
    (unwind-protect
        (apply original args)
      (push (list title
                  (mapcar #'buffer-file-name
                          (seq-filter (lambda (buffer)
                                        (buffer-local-value 'buffer-file-name buffer))
                                      (buffer-list))))
            filechooser-test-observations)
      (setq unread-command-events nil))))

(dolist (handler '(filechooser-handle-open-file filechooser-handle-save-files))
  (advice-add handler :around #'filechooser-test-request))

(filechooser-start)
(dbus-register-method
 :session "org.gnu.Emacs.FileChooser" "/org/freedesktop/portal/desktop"
 "org.gnu.Emacs.FileChooser.Verification" "Inspect"
 (lambda ()
   (json-encode
    `((implementation . ,(symbol-file 'filechooser-handle-open-file))
      (dired_kill_setting . ,(if dired-kill-when-opening-new-dired-buffer t :json-false))
      (visited_files . ,(vconcat (apply #'append (mapcar #'cadr filechooser-test-observations))))))))
