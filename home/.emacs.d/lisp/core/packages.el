;;; core/packages.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

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

  (eval-when-compile
    (require 'use-package))

;; These should be loaded early on
(straight-use-package '(org :type built-in))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save.")

(defvar use-package-config-hooks nil
  "Alist of hooks to run after a use-package config block.")

(defun add-use-package-config-hook (package hook)
  "Add a function to the list of hooks to be run after PACKAGE's use-package config."
  (let ((entry (assoc package use-package-config-hooks)))
    (if entry
        (push hook (cdr entry))
      (setq use-package-config-hooks
            (acons package (list hook) use-package-config-hooks)))))

(defun run-use-package-config-hooks (package)
  "Run all hooks for the given PACKAGE."
  (dolist (hook (cdr (assoc package use-package-config-hooks)))
    (funcall hook)))

(defmacro with-eval-after-use-package-config (package &rest body)
  "Specify actions to run after PACKAGE is configured via use-package."
  `(add-use-package-config-hook ,package (lambda () ,@body)))


(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)

(global-auto-revert-mode)
(diminish 'auto-revert-mode)

(use-package try
  :defer t
  :ensure t)

(use-package paradox
  :defer t
  :ensure t)

(provide 'core/packages)
;;; core/packages.el ends here
