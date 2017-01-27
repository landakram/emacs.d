;;; init.el --- Mark's emacs config
;;
;;; Commentary:
;;
;; Loads a literate config.
;;
;;; Code: 


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-config ()
  "Load the literate config."
  (require 'org)
  ;; Follow symlinks without prompting me.
  ;; This is here before loading ~config.org~ because ~config.org~ may
  ;; be symlinked.
  (setq vc-follow-symlinks t)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

(add-hook 'after-init-hook 'load-config)

(provide 'init)
