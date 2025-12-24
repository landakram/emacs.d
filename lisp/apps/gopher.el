;;; apps/gopher.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package elpher
  :straight t
  :after (general)
  :defer t
  :config
  (evil-set-initial-state 'elpher-mode 'motion)
  (general-define-key
   :keymaps '(elpher-mode-map)
   "C-j" 'evil-scroll-down
   "C-k" 'evil-scroll-up))

(provide 'apps/gopher)
;;; apps/gopher.el ends here
