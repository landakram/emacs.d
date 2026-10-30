;;; core/utils.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(defun eval-after-load-all (my-features form)
  "Run FORM after all MY-FEATURES are loaded.
See `eval-after-load' for the possible formats of FORM."
  (if (null my-features)
      (if (functionp form)
          (funcall form)
        (eval form))
    (eval-after-load (car my-features)
      `(lambda ()
         (eval-after-load-all
          (quote ,(cdr my-features))
          (quote ,form))))))

(defmacro with-eval-after-loads (my-features &rest body)
  (declare (indent 1) (debug (form def-body)))
  `(eval-after-load-all ,my-features (lambda () ,@body)))

(defun image-to-base64-data-url (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     (concat "data:" mime-type ";base64," base64-string)))

(defun image-to-base64 (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     base64-string))

(defvar use-package-config-hooks nil
  "Alist of hooks to run after a use-package config block.")

(defun add-use-package-config-hook (package hook)
  "Add a function to the list of hooks to be run after PACKAGE's use-package config."
  (let ((entry (assoc package use-package-config-hooks)))
    (if entry
        (setcdr entry (cons hook (cdr entry)))
      (push (cons package (list hook)) use-package-config-hooks))))

(defun run-use-package-config-hooks (package)
  "Run all hooks for the given PACKAGE."
  (dolist (hook (cdr (assoc package use-package-config-hooks)))
    (funcall hook)))

(defmacro with-eval-after-use-package-config (package &rest body)
  "Specify actions to run after PACKAGE is configured via use-package."
  `(add-use-package-config-hook ,package (lambda () ,@body)))

(defun my/load-host-config ()
  "Load the optional configuration for this machine."
  (let* ((name (concat (system-name) ".el"))
         (host-dir (expand-file-name "site-lisp/host-specific/" user-emacs-directory))
         (host-file (expand-file-name name host-dir))
         (legacy-file (expand-file-name (concat "site-lisp/" name) user-emacs-directory)))
    (when (and (file-readable-p legacy-file) (not (file-exists-p host-file)))
      (make-directory host-dir t)
      (rename-file legacy-file host-file))
    (when (file-readable-p host-file)
      (load-file host-file))))

(provide 'core/utils)
;;; core/utils.el ends here
