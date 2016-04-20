
(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; Packages

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;; Editor

; Stop prompting me to follow symlinks
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq ring-bell-function 'ignore)

(use-package better-defaults
  :ensure t)

(use-package ag
  :ensure t)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "env TERM=vt100 /bin/zsh -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  (global-visual-line-mode))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;;;; Theme

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (color-theme-sanityinc-tomorrow-eighties))

;;;; Projectile

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

;;;; Helm

(use-package helm
  :ensure t
  :config

  (require 'helm-config)
  (require 'helm-locate)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; Use helm for finding files
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  ;; Use helm for M-x
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; Use helm to show the kill ring
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; Use helm for buffer list
  (global-set-key (kbd "C-x b") 'helm-mini)

  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-open-github
  :ensure t)

;;;;;; Keybindings and evil mode

;; Keyboard shortcuts for increasing and decreasing text size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make horizontal movement cross lines               
  (setq-default evil-cross-lines t)
  (evil-mode 1)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-magit
    :ensure t)
  
  (use-package evil-leader
    :ensure t
    :config

    (evil-leader/set-leader "SPC")

    (evil-leader/set-key "x" 'helm-M-x)
    (evil-leader/set-key "f" 'helm-find-files)
    (evil-leader/set-key "w" 'save-buffer)
    (evil-leader/set-key "q" 'delete-window)
    (evil-leader/set-key "b" 'helm-mini)

    (evil-leader/set-key "pp" 'helm-projectile-switch-project)
    (evil-leader/set-key "pf" 'helm-projectile-find-file)
    (evil-leader/set-key "pa" 'helm-projectile-ag)

    ;; Config load
    (evil-leader/set-key "cl" 'eval-buffer)
    ;; Config edit
    (evil-leader/set-key "ce" (lambda () (interactive) (find-file (or user-init-file "~/.emacs.d/init.el"))))

    (evil-leader/set-key "g" 'magit-status)

    (setq evil-leader/in-all-states 1)
    (global-evil-leader-mode)))

;;;; Autocomplete

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;;; Org Mode

;; Allow exports with GitHub-flavored markdown

(use-package evil-org
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :config

  (use-package ox-gfm)

  (use-package org-habit)

  (use-package ox-odt
    :config
    (setq org-odt-preferred-output-format "rtf"))

  (use-package ox-jira
    :ensure t)

  (add-hook 'org-mode-hook #'my/org-mode)
  (add-hook 'org-agenda-mode-hook #'my/org-agenda-mode)

  (setq org-image-actual-width 300)
  (setq org-src-fontify-natively t)
  (setq org-log-done 'time)
  (setq org-agenda-files (quote ("~/org"
                                 )))

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")

  (setq org-startup-truncated 'nil)
  (setq org-catch-invisible-edits 'smart)
  (setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrgModeApp")

  (add-hook 'after-init-hook 'org-mobile-pull)
  (add-hook 'kill-emacs-hook 'org-mobile-push)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-use-fast-todo-selection t)
  (setq org-startup-indented t))

(use-package helm-org-rifle
  :ensure t)

(defun org-insert-subheading-after-current ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote))

(defun org-insert-subheading-after-current-and-enter-insert ()
  (interactive)
  (org-insert-subheading-after-current)
  (evil-append 0))

(defun org-insert-heading-after-current-and-enter-insert ()
  (interactive)
  (org-insert-heading-after-current)
  (evil-append 0))

(defun org-insert-todo-after-current-and-enter-insert ()
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-append 0))

(defun my/org-mode ()
  (evil-leader/set-key "*" 'org-ctrl-c-star)
  (evil-leader/set-key "a" 'org-agenda)
  (evil-leader/set-key "ih" 'org-insert-heading-after-current-and-enter-insert)
  (evil-leader/set-key "is" 'org-insert-subheading-after-current-and-enter-insert)
  (evil-leader/set-key "it" 'org-insert-todo-after-current-and-enter-insert)
  (evil-leader/set-key "n" 'org-narrow-to-subtree)
  (evil-leader/set-key "N" 'widen)
  (evil-leader/set-key "ml" 'org-do-demote)
  (evil-leader/set-key "mL" 'org-demote-subtree)
  (evil-leader/set-key "mh" 'org-do-promote)
  (evil-leader/set-key "mH" 'org-promote-subtree)
  (evil-leader/set-key "mk" 'org-metaup)
  (evil-leader/set-key "mj" 'org-metadown)
  (evil-leader/set-key "s" 'org-schedule)
  (evil-leader/set-key "t" 'org-todo))

(defun my/org-agenda-mode ()
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line))

(setq-default fill-column 85)
(setq-default left-margin-width 1)

;;;; Lisp

(use-package evil-cleverparens
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package sicp
  :ensure t)

(use-package geiser
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (enable-paredit-mode))

(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(defvar my/lisp-mode-hooks '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook clojure-mode-hook))

(dolist (mode my/lisp-mode-hooks)
  (add-hook mode #'enable-paredit-mode)
  (add-hook mode #'evil-cleverparens-mode))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'my/clojure-mode-hook))

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1))


;;;; Python

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-eshell)
  (venv-initialize-interactive-shells)
  (setq venv-location "~/.virtualenvs/"))

;;;; Elfeed

(use-package elfeed
  :ensure t
  :config

  (setq elfeed-feeds
        '("https://www.natashatherobot.com/feed/"
          "http://lambda-the-ultimate.org/rss.xml"
          "http://sachachua.com/blog/feed/"
          "http://airspeedvelocity.net/feed/")))

(setq url-queue-timeout 30)

;; Circe IRC

(use-package circe
  :ensure t
  :config
  
  (setq circe-network-options
        `(("Freenode"
           :nick "landakram"
           :channels (:after-auth
                      "#emacs"
                      "#clojure"
                      "#clojure-beginners"
                      "#iphonedev"
                      "#swift-lang")
           :nickserv-password "***REMOVED***"
           :reduce-lurker-spam t)))
  (enable-circe-color-nicks))

;; Magit

(use-package magit
  :ensure t)

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;; Yasnippet

(use-package yasnippet
  :ensure t)


;;; init.el ends here

;; Language modes

(use-package json-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package js-comint
  :ensure t)

(use-package haskell-mode
  :ensure t)

(provide 'init)
