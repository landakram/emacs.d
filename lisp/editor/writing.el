;;; editor/writing.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package olivetti
  :ensure t
  :defer t
  :diminish olivetti
  :config
  (setq-default olivetti-body-width 100))

(define-minor-mode write-mode
  "Write right"
  :lighter " write"
  (if (bound-and-true-p write-mode)
      (progn
        (diminish 'olivetti-mode)
        (diminish 'flyspell-mode)

        (olivetti-mode)
        (flyspell-mode))
    (progn
        (diminish-undo 'olivetti-mode)
        (diminish-undo 'flyspell-mode)

        (olivetti-mode -1)
        (flyspell-mode -1))))

(defun org-capture-write-mode ()
  "Enable write-mode for journal captures."
  (let ((key (org-capture-get :key)))
    (cond
     ((equal key "j")
      (write-mode 1)))))

(add-hook 'org-capture-mode-hook 'org-capture-write-mode)

(use-package gemini-mode
  :mode (("\\.gmi\\'" . gemini-mode))
  :straight t)

(provide 'editor/writing)
;;; editor/writing.el ends here
