;;; apps/budgeting.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :ensure t)

(provide 'apps/budgeting)
;;; apps/budgeting.el ends here
