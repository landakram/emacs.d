;;; lang/ruby.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package seeing-is-believing
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'seeing-is-believing))

  (use-package rbenv
    :ensure t
    :init
    (setq-default rbenv-installation-dir "/usr/local/Cellar/rbenv/1.1.2/")
    (defun my/ruby-init ()
      (rbenv-use-corresponding))
    (add-hook 'ruby-mode-hook 'my/ruby-init)
    :config
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)
    (rbenv-use-global)
)

(use-package rspec-mode
  :init
  (defun my/rspec-init ()
    (linum-mode -1)
    (local-set-key (kbd "r") 'inf-ruby-switch-from-compilation))
  (add-hook 'rspec-compilation-mode-hook 'my/rspec-init)
  :ensure t)

(use-package bundler
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.rbi$" . ruby-mode))

(provide 'lang/ruby)
;;; lang/ruby.el ends here
