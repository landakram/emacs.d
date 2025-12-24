;;; core/startup.el --- startup tweaks -*- lexical-binding: t; -*-

;;; Code:

(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq frame-resize-pixelwise t))

(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

(provide 'core/startup)
;;; core/startup.el ends here
