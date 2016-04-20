
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

(defvar my-packages)
(setq my-packages '(better-defaults
                      evil
                      helm
                      paredit
                      ag
                      helm-ag
                      evil-cleverparens
                      projectile
                      helm-projectile
                      magit
                      magit-gh-pulls
                      evil-magit
                      evil-leader
                      evil-org
                      evil-surround
                      org-plus-contrib
                      ox-jira
                      smart-mode-line
                      company
                      which-key
                      geiser
                      sicp
                      clojure-mode
                      json-mode
                      swift-mode
                      coffee-mode
                      js-comint
                      helm-open-github
                      cider
                      clj-refactor
                      virtualenvwrapper
                      color-theme-sanityinc-tomorrow
                      visual-fill-column
                      elfeed
                      circe
                      yasnippet
                      pdf-tools
                      haskell-mode
                      elpakit
                      helm-org-rifle
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Editor

; Stop prompting me to follow symlinks
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'visual-fill-column)
(global-visual-line-mode)

(require 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode)

(setq ring-bell-function 'ignore)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "env TERM=vt100 /bin/zsh -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;;; Theme

(require 'smart-mode-line)
(sml/setup)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-eighties)

;;;; Projectile

(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)


;;;; Helm

(require 'helm)
(require 'helm-config)
(require 'helm-locate)
(require 'helm-projectile)

(helm-projectile-on)

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


(helm-mode 1)


;;;;;; Keybindings and evil mode

;; Keyboard shortcuts for increasing and decreasing text size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader)
(require 'evil-magit)
(require 'evil-surround)

(global-evil-surround-mode 1)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
; Make horizontal movement cross lines                                    
(setq-default evil-cross-lines t)

(evil-leader/set-leader "<SPC>")

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
(global-evil-leader-mode)
(evil-mode 1)

;;;; Autocomplete

(add-hook 'after-init-hook 'global-company-mode)

;;;; Org Mode

;; Allow exports with GitHub-flavored markdown
(require 'ox-gfm)
(require 'org-habit)
(require 'ox-odt)
(require 'ox-confluence)
(require 'ox-jira)

(setq org-odt-preferred-output-format "rtf")

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

(setq-default fill-column 85)
(setq-default left-margin-width 1)

(add-hook 'text-mode-hook 'visual-fill-column-mode)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(defvar refile-path "~/org/refile.org")

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
(setq org-use-fast-todo-selection t)
(setq org-startup-indented t)

(setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrgModeApp")

(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

;;;; Paredit

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(defvar lisp '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook clojure-mode-hook))

(dolist (mode lisp)
  (add-hook mode #'enable-paredit-mode)
  (add-hook mode #'evil-cleverparens-mode))

(require 'clj-refactor)

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(add-hook 'clojure-mode-hook #'my/clojure-mode-hook)

;;;; Python

(require 'virtualenvwrapper)
(venv-initialize-eshell)
(venv-initialize-interactive-shells)
(setq venv-location "~/.virtualenvs/")

;;;; Elfeed

(require 'elfeed)
(setq elfeed-feeds
      '("https://www.natashatherobot.com/feed/"
        "http://lambda-the-ultimate.org/rss.xml"
        "http://sachachua.com/blog/feed/"
        "http://airspeedvelocity.net/feed/"
        ))


(setq url-queue-timeout 30)

;; Circe IRC

(require 'circe)
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

(eval-after-load "circe"
  '(progn
     (enable-circe-color-nicks)))

;; Magit
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Yasnippet


;; Org Rifle

(require 'helm-org-rifle)

;;; init.el ends here

(provide 'init)
