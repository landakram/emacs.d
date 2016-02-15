(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-enabled-themes (quote (smart-mode-line-light)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(fci-rule-color "#515151")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(defvar my-packages '(better-defaults
                      evil
                      helm
                      paredit
                      ag
                      helm-ag
                      evil-cleverparens
                      projectile
                      helm-projectile
                      magit
                      evil-magit
                      evil-leader
                      evil-org
                      org-plus-contrib
                      smart-mode-line
                      company
                      which-key
                      clojure-mode
                      cider
                      clj-refactor
                      color-theme-sanityinc-tomorrow
                      visual-fill-column))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Editor
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'visual-fill-column)
(global-visual-line-mode)

(require 'which-key)
(setq which-key-idle-delay 0.5)
(which-key-mode)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
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

(add-hook 'org-mode-hook (lambda ()
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
                           (evil-leader/set-key "t" 'org-todo)))
(add-hook 'org-agenda-mode-hook (lambda ()
    (define-key org-agenda-mode-map "j" 'evil-next-line)
    (define-key org-agenda-mode-map "k" 'evil-previous-line)))

(setq org-src-fontify-natively t)
(setq org-log-done 'time)
(setq org-agenda-files (quote ("~/org"
                                )))
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
;; (setq org-capture-templates
;;     (quote (("t" "todo" entry (file refile-path)
;;             "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;             ("r" "respond" entry (file refile-path)
;;             "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;             ("n" "note" entry (file refile-path)
;;             "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;             ("j" "Journal" entry (file+datetree refile-path)
;;             "* %?\n%U\n" :clock-in t :clock-resume t)
;;             ("w" "org-protocol" entry (file refile-path)
;;             "* TODO Review %c\n%U\n" :immediate-finish t)
;;             ("m" "Meeting" entry (file refile-path)
;;             "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;             ("Phone call" entry (file refile-path)
;;             "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;             ("h" "Habit" entry (file refile-path)
;;             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
(setq org-use-fast-todo-selection t)
(setq org-startup-indented t)

(setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrgModeApp")

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
  (add-hook mode 'enable-paredit-mode)
  (add-hook mode #'evil-cleverparens-mode))

(require 'clj-refactor)

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(add-hook 'clojure-mode-hook #'my/clojure-mode-hook)

(provide 'init)

;;; init.el ends here
