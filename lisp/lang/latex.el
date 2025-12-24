;;; lang/latex.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

(provide 'lang/latex)
;;; lang/latex.el ends here
