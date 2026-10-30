;;; browser-blame.el --- Idle line attribution in browsing checkouts -*- lexical-binding: t; -*-

(require 'blamer)

(defvar my/browser-blame-timer nil)
(defvar my/browser-blame-process nil)
(defvar-local my/browser-blame-key nil)
(defvar-local my/browser-blame-lines nil)
(defvar-local my/browser-blame-time 0)
(defvar my/browser-blame-overlay nil)

(defun my/browser-blame-eligible-p ()
  (and buffer-file-name (bound-and-true-p blamer-mode)
       (not (file-remote-p buffer-file-name)) (my/browser-root)
       (not (buffer-modified-p)) (not (use-region-p))
       (not (memq (bound-and-true-p evil-state) '(insert replace visual)))))

(defun my/browser-blame-clear ()
  (when (overlayp my/browser-blame-overlay)
    (delete-overlay my/browser-blame-overlay)
    (setq my/browser-blame-overlay nil)))

(defun my/browser-blame-hide-stale ()
  (when (and my/browser-blame-overlay
             (not (and (eq (overlay-buffer my/browser-blame-overlay) (current-buffer))
                       (eq (overlay-get my/browser-blame-overlay 'window) (selected-window))
                       (equal (overlay-end my/browser-blame-overlay) (line-end-position))
                       (my/browser-blame-eligible-p))))
    (my/browser-blame-clear)))

(defun my/browser-blame-parse ()
  "Read line numbers and commit metadata from Git's line-porcelain output."
  (goto-char (point-min))
  (let (lines)
    (while (re-search-forward "^\\([[:xdigit:]]+\\) [0-9]+ \\([0-9]+\\)\\(?: [0-9]+\\)?\n" nil t)
      (let* ((hash (match-string 1))
             (line (string-to-number (match-string 2)))
             (end (save-excursion (search-forward "\n\t") (point)))
             (field (lambda (name)
                      (save-excursion
                        (when (re-search-forward (concat "^" name " \\(.*\\)$") end t)
                          (match-string 1)))))
             (stamp (seconds-to-time (string-to-number (funcall field "author-time")))))
        (push (cons line (list :commit-hash hash :commit-author (funcall field "author")
                               :commit-date (format-time-string "%F" stamp)
                               :commit-time (format-time-string "%T" stamp)
                               :commit-message (truncate-string-to-width
                                                (funcall field "summary")
                                                blamer-max-commit-message-length nil nil "…")))
              lines)
        (goto-char end)))
    lines))

(defun my/browser-blame-show ()
  (with-current-buffer (window-buffer (selected-window))
    (when (and (my/browser-blame-eligible-p)
               (eq (car my/browser-blame-key) (current-buffer))
               (equal (nth 1 my/browser-blame-key) (buffer-chars-modified-tick)))
      (when-let* ((info (alist-get (line-number-at-pos nil t) my/browser-blame-lines)))
        (unless (and (overlayp my/browser-blame-overlay)
                     (eq (overlay-buffer my/browser-blame-overlay) (current-buffer))
                     (eq (overlay-get my/browser-blame-overlay 'window) (selected-window))
                     (equal (overlay-end my/browser-blame-overlay) (line-end-position))
                     (eq (overlay-get my/browser-blame-overlay 'blame-info) info))
          (my/browser-blame-clear)
          (when-let* ((text (blamer--create-popup-msg info)))
            (let ((ov (make-overlay (line-end-position) (line-end-position))))
              (overlay-put ov 'window (selected-window))
              (overlay-put ov 'blame-info info)
              (overlay-put ov 'before-string text)
              (setq my/browser-blame-overlay ov))))))))

(defun my/browser-blame-git ()
  (or (and my/browser-git-command-function (funcall my/browser-git-command-function (my/browser-root)))
      '("git")))

(defun my/browser-blame-start (key)
  (my/browser-blame-clear)
  (setq my/browser-blame-key key my/browser-blame-lines nil
        my/browser-blame-time (float-time))
  (let ((source (current-buffer))
        (default-directory (file-name-directory buffer-file-name))
        (output (generate-new-buffer " *browser blame*")))
    (condition-case err
        (setq my/browser-blame-process
              (make-process
               :name "browser-blame" :buffer output :connection-type 'pipe :noquery t
               :command (append (my/browser-blame-git)
                                (list "--no-optional-locks" "--literal-pathspecs" "blame"
                                      "--line-porcelain" "-L" (format "%d,+32" (nth 2 key))
                                      "--" (file-name-nondirectory buffer-file-name)))
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (unwind-protect
                       (when (buffer-live-p source)
                         (with-current-buffer source
                           (when (equal key my/browser-blame-key)
                             (setq my/browser-blame-time (float-time))
                             (when (zerop (process-exit-status process))
                               (setq my/browser-blame-lines
                                     (with-current-buffer output (my/browser-blame-parse)))
                               (my/browser-blame-show)))))
                     (when (buffer-live-p output) (kill-buffer output)))
                   ;; Pick up a different block if the user moved during the lookup.
                   (when (and (current-idle-time)
                              (>= (float-time (current-idle-time)) blamer-idle-time))
                     (my/browser-blame-idle))))))
      (file-error (kill-buffer output) (message "Blamer: %s" (error-message-string err))))))

(defun my/browser-blame-idle ()
  "Reuse nearby lines after a pause, with at most one Git lookup in flight."
  (with-current-buffer (window-buffer (selected-window))
    (when (my/browser-blame-eligible-p)
      (let* ((line (line-number-at-pos nil t))
             (start (1+ (* 32 (/ (1- line) 32))))
             (key (list (current-buffer) (buffer-chars-modified-tick) start buffer-file-name)))
        (if (and (equal key my/browser-blame-key)
                 (< (- (float-time) my/browser-blame-time) 60))
            (my/browser-blame-show)
          (unless (process-live-p my/browser-blame-process)
            (my/browser-blame-start key)))))))

(defun my/browser-blame-after-revert ()
  (when (eq (car my/browser-blame-key) (current-buffer))
    (when (and (overlayp my/browser-blame-overlay)
               (eq (overlay-buffer my/browser-blame-overlay) (current-buffer)))
      (my/browser-blame-clear))
    (setq my/browser-blame-key nil)
    (when (and (eq (window-buffer) (current-buffer)) (current-idle-time))
      (my/browser-blame-idle))))

(defun my/browser-blame-use-idle-hook ()
  ;; Blamer normally only installs these if VC registered the file during open.
  ;; Use the same idle path for checkout files regardless of their VC cache.
  (when (and buffer-file-name (my/browser-root))
    (remove-hook 'post-command-hook #'blamer--try-render t)
    (remove-hook 'window-state-change-hook #'blamer--try-render t)))

(add-hook 'blamer-mode-hook #'my/browser-blame-use-idle-hook)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer (my/browser-blame-use-idle-hook)))
(add-hook 'post-command-hook #'my/browser-blame-hide-stale)
(add-hook 'after-revert-hook #'my/browser-blame-after-revert)
(when (timerp my/browser-blame-timer) (cancel-timer my/browser-blame-timer))
(setq my/browser-blame-timer (run-with-idle-timer blamer-idle-time t #'my/browser-blame-idle))

(provide 'editor/browser-blame)
