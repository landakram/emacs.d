;;; init.el --- Mark's emacs config
;;
;;; Commentary:
;;
;; Loads a literate config.
;;
;;; Code: 

(defun load-config ()
  "Load the literate config."
  (require 'org)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

(add-hook 'after-init-hook 'load-config)

(provide 'init)
