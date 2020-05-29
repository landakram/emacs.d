;;; org-z.el -- Utilities for linking in org-mode

;; Copyright (C) 2020 Mark Hudnall

;; Author: Mark Hudnall <me@markhudnall.com>
;; URL: https://github.com/landakram/org-z
;; Keywords: org-mode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.3") (helm-org-rifle "1.7.1"))
;; Keywords: (outlines)

;;; Commentary:

;; Utilities for linking in org-mode

;;; Code:

(require 'org-id)
(require 'helm-org-rifle)

(defgroup org-z nil
  "org-z customizable variables."
  :group 'org)

(defcustom org-rifle-function 'helm-org-rifle-org-directory
  "The helm-org-rifle function to use when inserting links."
  :type 'function
  :group 'org-z)

(defun org-z-helm-org-rifle--store-link (candidate)
  "Store link to CANDIDATE."
  (-let (((buffer . pos) candidate))
    (save-excursion
      (with-current-buffer buffer
        (goto-char pos)
        (call-interactively 'org-store-link)))))

(defun org-z-helm-org-rifle--insert-link (candidate)
  "Insert link to CANDIDATE in current location."
  (interactive)
  (org-z-helm-org-rifle--store-link candidate)
  (call-interactively 'org-insert-link))

(defun org-z-insert-link ()
  "Insert link to CANDIDATE in current location."
  (interactive)
  (add-to-list 'helm-org-rifle-actions '("Insert link" . org-z-helm-org-rifle--insert-link))
  (funcall org-rifle-function)
  (pop helm-org-rifle-actions))

;;;###autoload
(define-minor-mode org-z-mode
  "Minor mode for org-z."
  :lighter " org-z"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-.") 'org-z-insert-link)
            map)
  :group 'org-z
  :require 'org-z
  :global t
  (when (not org-id-link-to-org-use-id)
    (setq org-id-link-to-org-use-id t)))

(provide 'org-z)

;;; org-z.el ends here
