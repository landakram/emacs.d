;;; lang/rust.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :ensure t
  :after general
  :hook ((rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred))
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t))


(with-eval-after-loads '(rust-mode lsp-mode)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-inlay-hint-enable t)
  (setq lsp-eldoc-render-all nil)

  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-inlay-hints-mode t))

(provide 'lang/rust)
;;; lang/rust.el ends here
