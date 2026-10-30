;;; lang/go.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               (cons "\\.go\\'" (if (treesit-language-available-p 'go) 'go-ts-mode 'go-mode)))
  (add-to-list 'auto-mode-alist
               (cons "/go\\.mod\\'" (if (treesit-language-available-p 'gomod)
                                         'go-mod-ts-mode 'conf-mode))))

(provide 'lang/go)
;;; lang/go.el ends here
