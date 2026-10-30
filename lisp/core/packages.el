;;; core/packages.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(require 'core/utils)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (setq use-package-ensure-function 'straight-use-package)

  (eval-when-compile
    (require 'use-package))

;; These should be loaded early on
(straight-use-package '(org :type built-in))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save.")

(use-package diminish
  :demand t
  :config
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode 1)
  (diminish 'auto-revert-mode))

(use-package try
  :defer t)

(use-package paradox
  :defer t)

(provide 'core/packages)
;;; core/packages.el ends here
