;;; apps/gpt.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package aidermacs
  :straight (aidermacs :type git :host github :repo "MatthewZMD/aidermacs" :branch "main")
  :bind (("C-c p" . aidermacs-transient-menu))

  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)

  :custom
  ; See the Configuration section below
  (aidermacs-auto-commits t)
  (aidermacs-default-model "o3-mini"))

(use-package gptel
  :straight t
  :bind
  (("s-k" . gptel-rewrite)
   ("s-j" . (lambda (arg) (interactive "p") (gptel-send arg)))))

(use-package agent-shell
  :straight t
  :bind
  (("s-l" . agent-shell-openai-start-codex))
  :config
  (setq agent-shell-thought-process-expand-by-default t))

(provide 'apps/gpt)
;;; apps/gpt.el ends here
