;;; editor/core.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


(setq vc-follow-symlinks t)


(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))


(defalias 'yes-or-no-p 'y-or-n-p)


(defconst my/viewer-source-modes
  '(prog-mode conf-mode yaml-mode yaml-ts-mode json-mode json-ts-mode
    nxml-mode web-mode toml-ts-mode dockerfile-mode))

(defun my/viewer-source-p ()
  (apply #'derived-mode-p my/viewer-source-modes))

(defun my/viewer-undo-history ()
  (when (my/viewer-source-p)
    (setq-local undo-tree-auto-save-history nil)))

(add-hook 'after-change-major-mode-hook #'my/viewer-undo-history)

(setq ring-bell-function 'ignore)

;; Lifted from the better-defaults package, with various things changed

(progn
  (unless (eq window-system 'ns)
    (menu-bar-mode -1)) 
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "~/.emacs.d/custom.el"))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups"))))))


(use-package ag
  :ensure t
  :defer t)

(setq create-lockfiles nil)

(defun delete-this-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (y-or-n-p (format "Are you sure you want to delete %s?" filename))
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer)))))))

(use-package session
  :ensure t
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file-coding-system 'utf-8))

(provide 'editor/core)
;;; editor/core.el ends here
