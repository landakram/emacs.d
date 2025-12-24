;;; lang/go.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package go-mode
  :ensure t
  :mode (("go\\.mod\\'" . go-mod-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :hook ((go-ts-mode . lsp-deferred))
  :config)

(use-package gotest
  :defer t
  :ensure t)

(provide 'lang/go)
;;; lang/go.el ends here
