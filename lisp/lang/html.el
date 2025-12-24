;;; lang/html.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(provide 'lang/html)
;;; lang/html.el ends here
