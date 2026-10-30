;;; lang/rust.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :init (setq rust-format-on-save nil))

(provide 'lang/rust)
;;; lang/rust.el ends here
