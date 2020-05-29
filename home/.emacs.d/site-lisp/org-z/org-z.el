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

(defcustom new-headings-file (concat org-directory "/" "new.org")
  "File in which to write new headings when inserting a link to a heading that does not already exist."
  :type 'file
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

(defun org-z--insert-link-to-new-heading (candidate)
  (let ((buf (find-file-noselect new-headings-file)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (condition-case nil
            (org-insert-heading-after-current)
          (error (org-insert-heading)))
        (org-edit-headline candidate)
        (save-buffer)
        (call-interactively 'org-store-link)))
    (call-interactively 'org-insert-link)))

(defvar org-z-insert-link--fallback
  (helm-build-dummy-source "Create link to new heading"
    :action (helm-make-actions
             "Insert link to new heading"
             #'org-z--insert-link-to-new-heading)))

;; These functions are basically just extracted from helm-org-rifle
(defun org-z--list-org-files (directories &optional toggle-recursion)
  (let* ((recursive (if (or toggle-recursion current-prefix-arg)
                        (not helm-org-rifle-directories-recursive)
                      helm-org-rifle-directories-recursive))
         (files (-flatten (--map (f-files it
                                          (lambda (file)
                                            (s-matches? helm-org-rifle-directories-filename-regexp (f-filename file)))
                                          recursive)
                                 directories))))
    (if files
        files
      (error "No org files found in directories: %s" (s-join " " directories)))))
(defun org-z--org-rifle-files-source (files)
  (--map (helm-org-rifle-get-source-for-file it) files))
(defun org-z--org-rifle-cleanup-hook ()
  (lambda ()
    ;; Close new buffers if enabled
    (when helm-org-rifle-close-unopened-file-buffers
      (if (= 0 helm-exit-status)
          ;; Candidate selected; close other new buffers
          (let ((candidate-source (helm-attr 'name (helm-get-current-source))))
            (dolist (source helm-sources)
              (unless (or (equal (helm-attr 'name source)
                                 candidate-source)
                          (not (helm-attr 'new-buffer source)))
                (kill-buffer (helm-attr 'buffer source)))))
        ;; No candidates; close all new buffers
        (dolist (source helm-sources)
          (when (helm-attr 'new-buffer source)
            (kill-buffer (helm-attr 'buffer source))))))))

(defun org-z-insert-link ()
  "Insert link to CANDIDATE in current location."
  (interactive)

  (let* ((helm-candidate-separator " ")
         (helm-cleanup-hook (org-z--org-rifle-cleanup-hook))
         (org-rifle-sources (org-z--org-rifle-files-source (org-z--list-org-files (list org-directory)))))
    (add-to-list 'helm-org-rifle-actions '("Insert link" . org-z-helm-org-rifle--insert-link))
    (helm :sources (append org-rifle-sources (list org-z-insert-link--fallback))
          :buffer "*helm sync source*")
    (pop helm-org-rifle-actions)))

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
