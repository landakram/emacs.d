;;; lang/python.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package python
  :ensure t
  :hook ((python-ts-mode . lsp-deferred))
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (make-local-variable 'python-shell-interpreter)
              (make-local-variable 'python-shell-interpreter-args)
              (when (executable-find "ipython")
                (setq python-shell-interpreter "ipython")
                (setq python-shell-interpreter-args "--simple-prompt")))))

(use-package python-black
  :straight t
  :after python
  :hook ((python-mode . python-black-on-save-mode)
         (python-ts-mode . python-black-on-save-mode)))

(use-package python-isort
  :straight t
  :hook (python-ts-mode . python-isort-on-save-mode))

(flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                              '("pyproject.toml" "ruff.toml" ".ruff.toml"))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://beta.ruff.rs/docs/'."
  :command ("ruff"
            "check"
            (config-file "--config" flycheck-python-ruff-config)
            "--format=text"
            "--stdin-filename" source-original
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))


(setq-default flycheck-checkers
                  (append flycheck-checkers
                          '(python-ruff)))

(use-package pytest
  :straight t
  :general
  (general-define-key
   :keymaps '(python-mode-map python-ts-mode-map)
   "C-c t ." 'pytest-one
   "C-c t f" 'pytest-module
   "C-c t p" 'pytest-all
   "C-c t r" 'pytest-again)
  :config
  ;; Use pyproject.toml to find a suitable project root for pytest
  ;; This is useful for monorepos
  (add-to-list 'pytest-project-root-files "pyproject.toml")
  ;; Don't use `-x -s` by default, we like capturing output
  (setq pytest-cmd-flags ""))

(use-package realgud
  :straight t

  :config
  (setq realgud:pdb-command-name "pytest --pdb"))

(provide 'lang/python)
;;; lang/python.el ends here
