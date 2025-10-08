;;; lang/markdown.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure t)

(provide 'lang/markdown)
;;; lang/markdown.el ends here
