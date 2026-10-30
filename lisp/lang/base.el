;;; lang/base.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(use-package jsonrpc :straight t :defer t)

(use-package corfu
  :straight t
  :defer 0.1
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 1)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary 'separator)

  (setq global-corfu-modes (list (cons 'not my/viewer-source-modes) t))
  (global-corfu-mode 1))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package yasnippet
  :ensure t
  :defer 0.1
  :config

  ;; Make Yasnippet work in Org
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  (add-hook 'yas-dont-activate-functions #'my/viewer-source-p)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(provide 'lang/base)
;;; lang/base.el ends here
