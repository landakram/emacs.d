;;; lang/lisp.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(defvar my/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook
    clojure-mode-hook
    janet-ts-mode-hook))

(use-package cider
  :ensure t
  :defer t
  :config
  (require 'general)

  (defun cider-system-reset ()
    "Call (user/reset)."
    (interactive)
    (save-excursion
      (cider-switch-to-repl-buffer)
      (goto-char cider-repl-input-start-mark)
      (delete-region (point) (point-max))
      (insert "(user/reset)")
      (cider-repl--send-input t)))

  (general-define-key
   :keymaps 'cider-mode-map
   "C-c r" 'cider-system-reset))

(use-package cljsbuild-mode
  :ensure t)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :ensure t)

(use-package sicp

  :ensure t)

(use-package geiser
  :straight t
  :init
  (setq geiser-active-implementations '(chicken guile)))

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :init
  (dolist (mode my/lisp-mode-hooks)
    (add-hook mode #'enable-paredit-mode)))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c RET"))

(add-hook 'clojure-mode-hook #'yas-minor-mode)

(use-package extempore-mode
  :ensure t)

(defvar keyword-lambda
  '(("(\\(lambda\\)\\>"
     (0 (prog1 () (compose-region
                   (match-beginning 1)
                   (match-end 1) ?λ))))))
(font-lock-add-keywords 'emacs-lisp-mode keyword-lambda)
(font-lock-add-keywords 'lisp-mode keyword-lambda)

;; Indenting module body code at column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(put 'module 'scheme-indent-function 'scheme-module-indent)

(put 'and-let* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(provide 'lang/lisp)
;;; lang/lisp.el ends here
