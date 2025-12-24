;;; tools/git.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package git-commit
  :straight (git-commit :type git :host github :repo "magit/magit")
  :after magit)

(use-package magit
  :straight t
  :commands (magit-get-current-branch)
  :defer t
  :config
  ;; Uncomment this to improve performance
  (setq magit-refresh-status-buffer nil)

  ;; If magit-refresh-status-buffer is nil, refresh the magit-status buffer on idle timer 
  (defun magit-refresh-on-idle-timer ()
    (when-let ((buffer (and (not (derived-mode-p 'magit-status-mode))
                            (magit-get-mode-buffer 'magit-status-mode))))
      (when (not magit-refresh-status-buffer)
        (run-with-idle-timer 2 nil (lambda (buffer)
                                     (message "Refreshing magit-status buffer %s" buffer)
                                     (with-current-buffer buffer (magit-refresh-buffer))) buffer))))

  (add-hook 'magit-post-refresh-hook 'magit-refresh-on-idle-timer)

  ;; (setq magit-refresh-verbose t)
  (setf magit-git-environment (append magit-git-environment '("FORCE_COLOR=0"))))

(defun magit-open-pull-request ()
  "Open the pull request on GitHub for the current branch."
  (interactive)
  (require 'magit)
  (browse-url (magit-pull-request-url)))

(defun magit-pull-request-url ()
  "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
  (format "%s/compare/%s"
          (replace-regexp-in-string
           (rx (and string-start (1+ any) "github.com:" (group (1+ any)) ".git" string-end))
           "https://github.com/\\1"
           (magit-get "remote" (magit-get-current-remote) "url"))
          (magit-get-current-branch)))

(defun parse-host-path-syntax (host-path-string)
  (let ((ssh-host-path-regex "\\(.*\\)\@\\(.*\\):\\(.*\\)"))
    (string-match ssh-host-path-regex host-path-string)
    (let ((user (match-string 1 host-path-string))
          (host (match-string 2 host-path-string))
          (path (match-string 3 host-path-string)))
      `((user . ,user)
        (host . ,host)
        (path . ,path)))))

(defun strip-dot-git (str)
  (replace-regexp-in-string "\.git$" "" str))

(defun valid-url? (str)
  (url-host (url-generic-parse-url str)))

(defun parse-url (str)
  (let ((url-obj (url-generic-parse-url str)))
    `((user . ,(url-user url-obj))
      (host . ,(url-host url-obj))
      (path . ,(url-filename url-obj)))))

(defun infer-https-url (str)
  (let* ((parsed-host-path (if (valid-url? str)
                               (parse-url str)
                               (parse-host-path-syntax str)))
        (host (alist-get 'host parsed-host-path))
        (path (alist-get 'path parsed-host-path)))
    (concat "https://" host "/" (strip-dot-git path))))

(defun get-remote-url (remote)
  (open-github--command-one-line "git" `("remote" "get-url" ,remote)))

(defun infer-browse-url-from-remote (remote)
  (let ((origin-url (get-remote-url remote)))
    (infer-https-url origin-url)))

(defun open-github--command-one-line (cmd args)
  (with-temp-buffer
    (when (zerop (apply 'call-process cmd nil t nil args))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun open-github--branch ()
  (let ((branch (open-github--command-one-line "git" '("symbolic-ref" "HEAD"))))
    (if (not branch)
        (error "Failed: 'git symbolic-ref HEAD'")
      (replace-regexp-in-string "\\`refs/heads/" "" branch))))

(defun open-github--highlight-marker (start end)
  (cond ((and start end (region-active-p))
         (format "#L%s..L%s" start end))
        (start
         (format "#L%s" start))
        (t "")))

(require 'subr-x)

(defun github-commit-url (commit-hash)
  (let* ((base-url (infer-browse-url-from-remote "origin")))
     (format "%s/commit/%s" base-url commit-hash)))

(defun github-file-url (&optional default-branch)
  (let* ((branch (if default-branch default-branch (open-github--branch)))
         (current-file (buffer-file-name))
         (root (vc-git-root current-file))
         (repo-path (file-relative-name current-file root))
         (base-url (infer-browse-url-from-remote "origin"))
         (start-line (line-number-at-pos (if (region-active-p) (region-beginning) (point))))
         (end-line (- (line-number-at-pos (region-end)) 1))
         (marker (open-github--highlight-marker start-line end-line)))
    (format "%s/blob/%s/%s%s" base-url branch repo-path marker)))

(defun github-url-save ()
  (interactive)
  (let ((url (github-file-url)) ) 
    (with-temp-buffer
      (insert url)
      (evil-yank (point-min) (point-max)))))

(defun github-open-file ()
  (interactive)
  (browse-url (github-file-url (if current-prefix-arg "master" nil))))

(defun org-store-github-link ()
  (interactive)
  (let* ((current-file (buffer-file-name))
         (root (vc-git-root current-file))
         (repo-path (file-relative-name current-file root))
         (github-link (github-file-url)))
    (add-to-list 'org-stored-links (list github-link repo-path))))

(use-package blamer
  :straight t
  :defer 1
  :config
  (setq blamer-force-truncate-long-line t)
  (setq blamer-max-commit-message-length 100)
  (setq blamer-idle-time 1)
  (setq blamer-tooltip-function 'blamer-tooltip-keybindings)

  (set-face-attribute 'blamer-face nil
                      :foreground (my/theme-color-get 'subtext1))

  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p t)
                                            (blamer--clear-overlay)))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p nil)
                                            (copilot-clear-overlay)))

  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash)
      ;; Split window vertically
      (let ((split-height-threshold nil)
            (split-width-threshold 0))
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (browse-url (github-commit-url commit-hash)))))

  (defun blamer-commit-into-at-point ()
    (let* ((line-number (line-number-at-pos))
           (file-name (blamer--get-local-name (buffer-file-name)))
           (blame-cmd-res (when file-name
                            (apply #'vc-git--run-command-string file-name
                                   (append blamer--git-blame-cmd
                                           (list (format "%s,%s" line-number line-number))))))
           (blame-cmd-res (when blame-cmd-res (butlast (split-string blame-cmd-res "\n")))))
      (blamer--parse-line-info (first blame-cmd-res) nil)))

  (defun blamer-open-remote-at-point ()
    (interactive)
    (let ((commit-info (blamer-commit-into-at-point)))
      (blamer-callback-open-remote commit-info)))

  (defun blamer-open-magit-at-point ()
    (interactive)
    (let ((commit-info (blamer-commit-into-at-point)))
      (blamer-callback-show-commit-diff commit-info)))

  (leader-def ".go" 'blamer-open-remote-at-point)
  (leader-def ".gc" 'blamer-open-magit-at-point)

  (setq blamer-type 'visual)

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

(provide 'tools/git)
;;; tools/git.el ends here
