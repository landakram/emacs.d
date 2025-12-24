;;; init.el --- entrypoint -*- lexical-binding: t; -*-

;; Ensure our Lisp directory is available even in batch/non-standard setups.
(let* ((root (file-name-directory (file-truename (or load-file-name user-init-file))))
       (lisp-dir (expand-file-name "lisp" root)))
  (add-to-list 'load-path lisp-dir))

(require 'core/init)

;;; init.el ends here
