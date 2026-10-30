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

(my/load-host-config)

(provide 'os/env)
;;; os/env.el ends here
