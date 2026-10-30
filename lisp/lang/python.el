;;; lang/python.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package realgud
  :straight t
  :commands realgud:pdb
  :config
  (setq realgud:pdb-command-name "pytest --pdb"))

(provide 'lang/python)
;;; lang/python.el ends here
