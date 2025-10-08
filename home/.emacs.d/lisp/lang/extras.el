;;; lang/extras.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package nix-mode
  :straight (nix-mode :type git :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'")

(use-package nix-sandbox
  :ensure t
  :config
  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args))))

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :straight (solidity-flycheck :type git :host github :repo "ethereum/emacs-solidity")
  :hook solidity-mode
  :config
  (setq solidity-flycheck-chaining-error-level t)
  (setq solidity-flycheck-use-project t))

(use-package prettier
  :straight t
  :config)

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

(setq treesit-language-source-alist
      (if (eq 'windows-nt system-type)
          '((janet-simple
             . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                nil nil "gcc.exe")))
        '((janet-simple
           . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

(when (not (treesit-language-available-p 'janet-simple))
  (treesit-install-language-grammar 'janet-simple))

(use-package janet-ts-mode
  :straight (janet-ts-mode :type git :host github :repo "sogaiu/janet-ts-mode")
  :mode (("\\.janet\\'" . janet-ts-mode)))

(use-package ajrepl
  :straight (ajrepl :host github
                    :repo "sogaiu/ajrepl"
                    :files ("*.el" "ajrepl"))
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

(with-eval-after-loads '(janet-ts-mode lsp-mode) 
  (add-to-list 'lsp-language-id-configuration
               '(janet-ts-mode . "janet"))

  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "janet-lsp")
                        :activation-fn (lsp-activate-on "janet")
                        :server-id 'janet-lsp)))

(with-eval-after-loads '(janet-ts-mode)
  (require 'reformatter)

  (defgroup janet-format nil
    "Python reformatting using black."
    :group 'janet
    :prefix "janet-format-")

  (reformatter-define janet-format
    :program "janet-format"
    :group 'janet-format)

  (add-hook 'janet-ts-mode-hook 'janet-format-on-save-mode))

(use-package kubed
  :straight (kubed :type git :host github :repo "eshelyaron/kubed"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'lang/extras)
;;; lang/extras.el ends here
