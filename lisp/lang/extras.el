;;; lang/extras.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package nix-mode
  :straight (nix-mode :type git :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'")

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash))

(use-package vyper-mode
  :straight t)

(use-package verb
  :straight t
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((verb . t))))
  (setq verb-auto-kill-response-buffers t)
  )

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'" . terraform-mode))

(use-package janet-ts-mode
  :straight (janet-ts-mode :type git :host github :repo "sogaiu/janet-ts-mode")
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               (cons "\\.janet\\'" (if (treesit-language-available-p 'janet-simple)
                                        'janet-ts-mode 'prog-mode))))

(use-package kubed
  :defer t
  :straight (kubed :type git :host github :repo "eshelyaron/kubed"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'lang/extras)
;;; lang/extras.el ends here
