;;; os/env.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package exec-path-from-shell
 :ensure t
 :config
 (setq exec-path-from-shell-arguments '("-l"))
 (setq exec-path-from-shell-variables '("PATH"
                                        "MANPATH"
                                        "NIX_PATH"
                                        "SSH_AGENT_PID"
                                        "SSH_AUTH_SOCK"))
 (exec-path-from-shell-initialize))

(defun my/gpg-agent ()
  "Load your gpg-agent.env file into the environment when present.

This is extra useful if you use gpg-agent with --enable-ssh-support."
  (interactive)
  (let* ((home (getenv "HOME"))
         (info-file (expand-file-name ".gpg-agent-info" home)))
    (when (file-readable-p info-file)
      (with-temp-buffer
        (insert-file-contents info-file)
        (goto-char (point-min))
        (setq case-replace nil)
        (replace-regexp "\\(.*\\)=\\(.*\\)" "(setenv \"\\1\" \"\\2\")")
        (eval-buffer))
      (message (getenv "GPG_AGENT_INFO")))))

(run-with-idle-timer 60 t #'my/gpg-agent)
(my/gpg-agent)

(let* ((host-dir (expand-file-name "~/.emacs.d/site-lisp/host-specific"))
       (host-file (expand-file-name (concat (system-name) ".el") host-dir))
       (legacy-file (expand-file-name (concat "~/.emacs.d/site-lisp/" (system-name) ".el"))))
  ;; Migrate legacy host file into host-specific/ if still present.
  (when (and (file-readable-p legacy-file)
             (not (file-readable-p host-file)))
    (message "Migrating legacy host file %s to %s" legacy-file host-file)
    (make-directory host-dir t)
    (rename-file legacy-file host-file t))

  (when (file-directory-p host-dir)
    (message "Loading host-specific config from %s" host-dir)
    ;; Load host-named file first (if present), then any other *.el for the host.
    (when (file-readable-p host-file)
      (message "Loading host file %s" host-file)
      (load-file host-file))
    (dolist (file (directory-files host-dir t "\\.el$"))
      (unless (string= file host-file)
        (message "Loading host file %s" file)
        (load-file file)))))

(provide 'os/env)
;;; os/env.el ends here
