;;; lang/yaml.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(provide 'lang/yaml)
;;; lang/yaml.el ends here
