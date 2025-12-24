;;; lang/vim.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package vimrc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tridactyl\\(rc\\)?\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(provide 'lang/vim)
;;; lang/vim.el ends here
