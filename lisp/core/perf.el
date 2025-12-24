;;; core/perf.el --- performance tuning -*- lexical-binding: t; -*-

;;; Code:

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq jit-lock-defer-time 0.05)

(use-package gcmh
  :straight t
  :config
  (setq gcmh-high-cons-threshold (* 1024 1024 1024))
  (setq gcmh-idle-delay-factor 20)
  (gcmh-mode 1))

(provide 'core/perf)
;;; core/perf.el ends here
