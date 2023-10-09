(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq frame-resize-pixelwise t))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save.")

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)

(global-auto-revert-mode)
(diminish 'auto-revert-mode)

(use-package try
  :defer t
  :ensure t)

(use-package paradox
  :defer t
  :ensure t)

(use-package mood-line
  :straight t
  :config
  (set-face-attribute 'mood-line-buffer-name
                      nil
                      :weight 'bold)
  (mood-line-mode))

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'base16-shell)
  (setq base16-distinct-fringe-background nil)
  (load-theme 'base16-material-darker t)

  (defvar my/base16-colors base16-material-darker-theme-colors))

;; (use-package monokai-theme
;;   :ensure t
;;   :config
;;   (setq monokai-use-variable-pitch nil)
;;   (load-theme 'monokai t))


;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (color-theme-sanityinc-tomorrow-eighties))

;; (use-package color-theme
;;   :ensure t)


;; (use-package load-theme-buffer-local
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda ()
;;                              (load-theme-buffer-local
;;                               'leuven
;;                               (current-buffer)))))

;; (use-package color-theme-buffer-local
;;   :ensure t)

(setq vc-follow-symlinks t)

(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :diminish flycheck-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)

  ;; Disable syntax checking on new-line for emacs lisp, since for some reason 
  ;; it is really slow
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically '(idle-check mode-enabled save))))


  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint
                          python-flake8
                          ruby-rubocop
                          ruby-reek
                          emacs-lisp-checkdoc))))

(setq ring-bell-function 'ignore)

;; Lifted from the better-defaults package, with various things changed

(progn
  (unless (eq window-system 'ns)
    (menu-bar-mode -1)) 
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "~/.emacs.d/custom.el"))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups"))))))

(use-package ag
  :ensure t
  :defer t)

;; (use-package visual-fill-column
;;   :ensure t
;;   :diminish visual-line-mode
;;   :config
;;   (add-hook 'text-mode-hook 'visual-fill-column-mode)
;;   (global-visual-line-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(setq create-lockfiles nil)

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
  "Load your gpg-agent.env file in to the environment

This is extra useful if you use gpg-agent with --enable-ssh-support"
  (interactive)
  (let ((home (getenv "HOME"))
        (old-buffer (current-buffer)))
    (with-temp-buffer
      (insert-file-contents (concat home "/.gpg-agent-info"))
      (goto-char (point-min))
      (setq case-replace nil)
      (replace-regexp "\\(.*\\)=\\(.*\\)" "(setenv \"\\1\" \"\\2\")")
      (eval-buffer)))
  (message (getenv "GPG_AGENT_INFO")))

(run-with-idle-timer 60 t 'my/gpg-agent)
(my/gpg-agent)

(defun delete-this-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (y-or-n-p (format "Are you sure you want to delete %s?" filename))
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer)))))))

(let ((host-specific-config (expand-file-name (concat "~/.emacs.d/site-lisp/" (system-name) ".el")))) 
  (when (file-readable-p host-specific-config)
    (load-file host-specific-config)))

(use-package session
  :ensure t
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file-coding-system 'utf-8))

(ido-mode -1)

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys)
  (setq general-default-states '(normal motion))

  (general-create-definer leader-def
    :prefix "SPC"
    :keymaps '(normal motion override))

  (leader-def :infix "b"
    "" '(:ignore t :which-key "buffers")
    "k" 'kill-this-buffer
    "q" 'delete-window)

  ;; Bookmarks
  (leader-def :infix "bo"
    "" '(:ignore t :which-key "b[o]okmarks")
    "c" '(:which-key "config-file"
                     :def (lambda () (interactive) (find-file "~/.emacs.d/config.org")))
    "m" '(mu4e :which-key "mu4e")
    "s" '(:which-key "stump"
                       :def (lambda () (interactive) (find-file "~/.stumpwmrc")))
    "o" '(:which-key "org-file"
                     :def (lambda () (interactive) (find-file "~/org/projects.org")))
    "g" '(:which-key "goldfinch"
                     :def (lambda () (interactive) (find-file "~/org/goldfinch.org"))))

  (leader-def :infix "w"
    "" '(:ignore t :which-key "windows")
    "d" 'ace-delete-window
    "j" 'ace-window)

  (leader-def
    "|" 'split-window-right-and-focus
    "-" 'split-window-below-and-focus)

  (leader-def
    "a" 'org-agenda)

  (leader-def
    "g" 'magit-status)

  ;; Help
  (leader-def
    "h" (general-simulate-key "C-h"))

  (leader-def :infix "f"
    "" '(:ignore t :which-key "files")
    "d" 'delete-this-file
    "c" 'xah-copy-file-path
    "s" 'save-buffer)

  ;; Clojure
  (general-define-key :keymaps 'cider-mode-map
                      "gf" 'cider-find-dwim)

  ;; Clojure shortcuts
  (leader-def :infix ","
    :keymaps 'clojure-mode-map
    "" '(:ignore t :which-key "Mode-specific")
    "c" 'cider
    "i" 'cider-inspect
    "e" 'cider-eval-defun-at-point
    "b" 'cider-eval-buffer
    "r" 'cider-switch-to-repl-buffer 
    "s" 'cider-selector)

  (general-define-key :keymaps 'cider-stacktrace-mode-map
                      "q" 'cider-popup-buffer-quit-function)

  (general-define-key :states '(emacs normal motion)
                      "C-x k" 'kill-this-buffer)

  (general-define-key :states '(emacs) :keymaps 'org-agenda-mode-map
                      "j" 'org-agenda-next-line
                      "k" 'org-agenda-previous-line)

  (leader-def
    :states '(normal)
    :keymaps 'outline-minor-mode-map
    "N" 'widen)

  (general-define-key :states '(normal)
                      :keymaps 'outline-minor-mode-map
                      "M-j" 'outline-next-visible-heading
                      "M-k" 'outline-previous-visible-heading
                      "M-K" 'outline-backward-same-level
                      "M-J" 'outline-forward-same-level)

  (leader-def :infix "p"
    "" '(:ignore t :which-key "projects")
    "p" 'projectile-switch-project
    "f" 'projectile-find-file
    "t" 'projectile-test-project)

  (general-define-key
   :states '(normal)
   "C-k" (lambda ()
           (interactive)
           (evil-scroll-up nil))
   "C-j" (lambda ()
           (interactive)
           (evil-scroll-down nil)))

  (general-define-key
   :states '(normal)
   :keymaps '(evil-normal-state-map org-mode-map)
   "C-k" (lambda ()
           (interactive)
           (evil-scroll-up nil))
   "C-j" (lambda ()
           (interactive)
           (evil-scroll-down nil)))

  (general-define-key
   :states '(normal)
   :keymaps 'pdf-view-mode-map
   "C-k" 'pdf-view-next-page-command
   "C-j" 'pdf-view-previous-page-command) 

  (general-define-key
   :states '(normal)
   "f" 'avy-goto-word-or-subword-1))

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(defun split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (other-window 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory
                                                     "undo-tree"))))
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :diminish evil-mode
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; Make movement keys work over visual lines
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  (evil-set-undo-system 'undo-tree)

  ;; Make * search over whole symbols instead of words. This means 
  ;; it will match "this-variable" rather than just "this".
  (setq-default evil-symbol-word-search 1)
  (setq-default evil-want-fine-undo t)

  ;; Make insert mode just like regular emacs
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)


  (setq evil-move-cursor-back nil)

  (evil-mode 1)

  (use-package evil-surround
    :straight t
    :diminish evil-surround-mode
    :config
    (global-evil-surround-mode 1))

  (use-package evil-matchit
    :straight t
    :config
    (global-evil-matchit-mode 1)))

  (use-package evil-collection
    :after evil
    :straight t
    :config
    (evil-collection-init))

(use-package jumpy
  :general
  (:states '(motion)
           "C-o" 'jumpy-back
           "C-i" 'jumpy-forward)
  :straight (jumpy :type git
                     :host github
                     :repo "landakram/jumpy"
                     :branch "master")
  :config
  (global-jumpy-mode t)
  (setq jumpy-buffer-filters
        '("\\*Messages\\*"
            "Output\\*$"
            help-mode
            compilation-mode
            magit-mode
            magit-status-mode
            magit-diff-mode))
  (setq jumpy-prefer-same-window t))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode)
  (setq projectile-completion-system 'default)
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-indexing-method 'hybrid)

  ;; Clear out all commander commands but the help item.
  (setq projectile-commander-methods (list (car projectile-commander-methods)))
  ;; (delete-if (lambda (el)
  ;;              (member (car el) '(?d ?a ?g)))
  ;;            projectile-commander-methods)


  ;; Use ag instead of projectile's default of find.
  ;; This lets me use .agignore files instead of projectile's
  ;; ignore file, which has never worked successfully for me.
  (setq projectile-generic-command
        (concat "ag -0 -l --nocolor"
                (mapconcat #'identity (cons "" projectile-globally-ignored-directories) " --ignore-dir=")))

  ;; Workaround for tramp slowness (https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh)
  (setq projectile-mode-line "Projectile")

  (def-projectile-commander-method ?d
    "Open project root in dired"
    (projectile-dired)))

  (def-projectile-commander-method ?e
    "Open an [e]shell in the project root."
    (projectile-run-eshell))

  (def-projectile-commander-method ?f
    "Find files in the project."
    (projectile-find-file))

  (def-projectile-commander-method ?g
    "Open project root in magit"
    (projectile-vc))

(use-package avy
  :ensure t
  :config
  ;; Favor home-row and surrounding keys
  (setq avy-keys
        '(?h ?j ?k ?l ?a ?s ?d ?f ?g ?y ?u ?i ?o ?p ?q ?w ?e ?r ?t ?n ?m ?z ?x ?c ?v ?b)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g ?y ?u ?i ?o ?p ?q ?w ?e ?r ?t ?n ?m ?z ?x ?c ?v ?b)))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(winner-mode 1)

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(use-package dumb-jump
  :straight t
  :commands (dumb-jump-xref-activate)
  :init 
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-force-searcher 'rg))

(setq tramp-verbose 6)
(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options "")
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
(setq tramp-auto-save-directory temporary-file-directory)

;; TODO: unfortunately this doesn't quite work
  (defun tail-this-file ()
    (interactive)
    (dired-do-shell-command "tail -f * &" nil (dired-get-marked-files)))

(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

(defun my/shell-set-hook ()
    (when (file-remote-p (buffer-file-name))
      (let ((vec (tramp-dissect-file-name (buffer-file-name))))
       ;; Please change "some-hostname" to your remote hostname
        (setq-local shell-file-name "/bin/bash")
        ;; (when (string-match-p "some-hostname" (tramp-file-name-host vec))
        ;;  (setq-local shell-file-name "/bin/bash")
        )))
        
(add-hook 'find-file-hook #'my/shell-set-hook)

(defvar outline-minor-mode-prefix "\M-#")

(use-package outshine
  :general
  (:states '(normal) :keymaps 'outline-minor-mode-map
           "TAB" 'outshine-cycle
           "<backtab>" 'outshine-cycle-buffer)
  (leader-def
    :states '(normal)
    :keymaps 'outline-minor-mode-map
    "n" 'outshine-narrow-to-subtree)
  :ensure t)

(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-01-27"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (buffer-file-name)
               (buffer-file-name)
             (user-error "Current buffer is not associated with a file.")))))
    (kill-new
     (if *dir-path-only-p
         (progn
           (message "Directory path copied: %s" (file-name-directory -fpath))
           (file-name-directory -fpath))
       (progn
         (message "File path copied: %s" -fpath)
         -fpath )))))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package dired
  :general
  ("C-x j" 'dired-jump)
  (leader-def
    "d" 'dired-jump)
  (:keymaps
   'dired-mode-map
   "h" 'dired-up-directory
   "l" 'dired-find-file)
  :config
  (setq dired-listing-switches "-alh")
  )

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode)

  (set-face-attribute 'diredfl-dir-priv nil
                      :foreground (plist-get my/base16-colors :base0D)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-read-priv nil
                      :foreground (plist-get my/base16-colors :base0B)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-write-priv nil
                      :foreground (plist-get my/base16-colors :base0A)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-exec-priv nil
                      :foreground (plist-get my/base16-colors :base08)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-no-priv nil
                      :foreground (plist-get my/base16-colors :base03)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-dir-name nil
                      :foreground (plist-get my/base16-colors :base0C)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-symlink nil
                      :foreground (plist-get my/base16-colors :base05)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-dir-heading nil
                      :weight 'bold
                      :foreground (plist-get my/base16-colors :base0B)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-file-name nil
                      :foreground (plist-get my/base16-colors :base05)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-file-suffix nil
                      :foreground (plist-get my/base16-colors :base0B)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-number nil
                      :foreground (plist-get my/base16-colors :base0A)
                      :background (plist-get my/base16-colors :base00))

  (set-face-attribute 'diredfl-date-time nil
                      :foreground (plist-get my/base16-colors :base0D)
                      :background (plist-get my/base16-colors :base00)))

;;(use-package json-navigator
;;  :straight t)

(global-so-long-mode t)

;;(add-to-list 'default-frame-alist
;;             '(font . "Fira Code Medium-12"))

(set-face-attribute 'default nil
                    :family "Fira Code" :height 120 :weight 'normal)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

(when (member "Twemoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Twemoji") nil 'prepend))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq help-window-select t)

(defun my-org-pop-to-buffer (orig-fn buf &optional norecord)
  (if shackle-mode
      (pop-to-buffer buf nil norecord)
    (funcall orig-fn buf norecord)))
(advice-add 'org-switch-to-buffer-other-window :around #'my-org-pop-to-buffer)

(defun my-suppress-delete-other-windows (orig-fn &rest args)
  (if shackle-mode
      (letf (((symbol-function 'delete-other-windows) #'ignore)
             ((symbol-function 'delete-window)        #'ignore))
        (apply orig-fn args))
    (apply orig-fn args)))

(setq org-agenda-window-setup 'other-window)

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules
        '((help-mode :align below
                     :select t
                     :size 0.4
                     :popup t)
          (compilation-mode :align below
                            :select t
                            :size 0.3
                            :popup t)
          (" *Agenda Commands*"
           :align below
           :size 0.4
           :popup t)
          ("*Org Agenda*" :align below :popup t :size 0.4)))
  (shackle-mode))

(use-package popper
  :ensure t
  :general
  (general-define-key
   "C-`" 'popper-toggle-latest
   "M-`" 'popper-cycle)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          help-mode
          compilation-mode))
  (popper-mode +1)
  :config
  (setq popper-display-control nil))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult" :branch "main")
  :after projectile
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g l" . consult-line)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g r" . consult-git-grep)
         ("M-g f" . consult-find)
         ("M-g i" . consult-project-imenu)
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (setq consult-narrow-key "<")

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (leader-def :infix "b"
    "b" 'consult-buffer)

  (defun consult-ripgrep-at-point ()
    (interactive)
    (consult-ripgrep default-directory (thing-at-point 'symbol)))

  (defun consult-project-ripgrep-at-point ()
    (interactive)
    (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

  (leader-def :infix "p"
    "a" 'consult-project-ripgrep-at-point)

  (defun consult--buffer-sort-visibility-in-other-windows (buffers)
    "Sort BUFFERS by visibility, only excluding a visibile buffer if its in the current window."
    (let ((hidden)
          (current (current-buffer)))
      (consult--keep! buffers
        (unless (eq it current)
          (if
              (eq (get-buffer-window it 'visible)
                  (selected-window))
              it
            (push it hidden)
            nil)))
      (nconc (nreverse hidden) buffers (list (current-buffer)))))

  ;; Overriding to change the 'visibility sort. This makes the last visited buffer
  ;; appear in the buffer list, even if it is open in a different window.
  (setq consult--source-buffer
        `(:name     "Buffer"
                    :narrow   ?b
                    :category buffer
                    :face     consult-buffer
                    :history  buffer-name-history
                    :state    ,#'consult--buffer-state
                    :default  t
                    :items
                    ,(lambda () (consult--buffer-query :sort 'visibility-in-other-windows
                                                  :as #'buffer-name))))

  (def-projectile-commander-method ?a
    "Full text search in the project."
    (consult-project-ripgrep-at-point))

  (add-hook 'eshell-mode-hook
            (lambda()
              (define-key eshell-mode-map (kbd "M-r") 'consult-history))))

(use-package consult-imenu
  :straight (consult-imenu :type git :host github :repo "minad/consult" :branch "main")
  :general (general-define-key
            :states '(normal)
            "F" 'consult-imenu))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

  :config
  (leader-def :infix "f"
    "f" 'find-file)

  (leader-def 
    "x" 'execute-extended-command)

  (leader-def :infix "b"
    "b" 'consult-buffer)
  )


;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :straight (consult-flycheck :type git :host github :repo "minad/consult" :branch "main")
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :bind
  (:map electrum-minibuffer-map
        ("C-j" . embark-act))

  :config
  ;; Pop up which-key when running embark-act
  (setq embark-action-indicator
        (lambda (map &optional _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  (setq embark-prompter 'embark-keymap-prompter))

(use-package embark-consult
  :straight (embark-consult :type git :host github :repo "oantolin/embark")
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight (marginalia :type git :host github :branch "main" :repo "minad/marginalia")
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

(column-number-mode)

(set-fringe-mode 10)

(use-package beacon
  :straight t
  :config
  (beacon-mode 1)
  (setq beacon-color (plist-get my/base16-colors :base02)))

(use-package dimmer
  :straight t
  :config
  (dimmer-mode t)
  (dimmer-configure-which-key)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)

  (defun advices/dimmer-config-change-handler ()
    (dimmer--dbg-buffers 1 "dimmer-config-change-handler")
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (dimmer-process-all (not ignore))))
  (advice-add 'dimmer-config-change-handler :override #'advices/dimmer-config-change-handler)

  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*Help\\*")
  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*compilation\\*")
  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*mu4e-headers\\*")
  (add-to-list 'dimmer-buffer-exclusion-regexps "\\*mu4e-view\\*"))

(use-package olivetti
  :ensure t
  :defer t
  :diminish olivetti
  :config
  (setq-default olivetti-body-width 100))

(define-minor-mode write-mode
  "Write right"
  :lighter " write"
  (if (bound-and-true-p write-mode)
      (progn
        (diminish 'olivetti-mode)
        (diminish 'flyspell-mode)

        (company-mode -1)
        (olivetti-mode)
        (flyspell-mode))
    (progn
        (diminish-undo 'olivetti-mode)
        (diminish-undo 'flyspell-mode)

        (company-mode)
        (olivetti-mode -1)
        (flyspell-mode -1))))

(defun org-capture-write-mode ()
  "Enable write-mode for journal captures."
  (let ((key (org-capture-get :key)))
    (cond
     ((equal key "j")
      (write-mode 1)))))

(add-hook 'org-capture-mode-hook 'org-capture-write-mode)

(use-package gemini-mode
  :mode (("\\.gmi\\'" . gemini-mode))
  :straight t)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)

  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (setq copilot-max-char 200000)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package tdd
  :load-path "site-lisp/tdd/")

(defun local/postprocess-compilation-buffer ()
  (goto-char compilation-filter-start)
  (when (looking-at "\033c")
    (delete-region (point-min) (match-end 0)))
  (ansi-color-apply-on-region (point) (point-max)))

(add-hook 'compilation-filter-hook 'local/postprocess-compilation-buffer)

(setq compilation-scroll-output 'first-error)

(defun compilation-mode-common-search-paths (orig-fn &rest args)
  (let* ((project-root (car (project-roots (project-current))))
         (compilation-search-path
          (list
           project-root
           (concat (file-name-as-directory project-root) "node_modules"))))
    (prin1 compilation-search-path)
    (apply orig-fn args)))

(advice-add 'compilation-find-file :around #'compilation-mode-common-search-paths)

(add-to-list 'compilation-error-regexp-alist 'mocha)
(add-to-list 'compilation-error-regexp-alist 'mocha-abs)

(add-to-list 'compilation-error-regexp-alist-alist
             '(mocha "at.*(\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\))" 1 2 3))

(add-to-list 'compilation-error-regexp-alist-alist
             '(mocha-abs "at \\([^ ]+?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode)

  (add-to-list 'dtrt-indent-hook-mapping-list '(scss-mode css css-indent-offset))
  (add-to-list 'dtrt-indent-hook-mapping-list '(solidity-mode c/c++/java c-basic-offset)))

(use-package company
  :ensure t
  :defer 0.1
  :config
  (setq company-idle-delay 0) 
  (setq company-minimum-prefix-length 1)
  (setq company-global-modes '(not org-mode eshell-mode))
  (global-company-mode)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package magit
  :straight t
  :commands (magit-get-current-branch)
  :defer t
  :config
  ;; Uncomment this to improve performance
  ;; (setq magit-refresh-status-buffer nil)
  ;; (setq magit-refresh-verbose t)
  (setf magit-git-environment (append magit-git-environment '("FORCE_COLOR=0"))))

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

(use-package yasnippet
  :ensure t
  :defer 0.1
  :config

  ;; Make Yasnippet work in Org
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(defvar my/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook
    clojure-mode-hook))

(use-package evil-cleverparens
  :ensure t
  :commands (evil-cleverparens-mode)
  :init
  (dolist (mode my/lisp-mode-hooks)
    (add-hook mode #'evil-cleverparens-mode)))

(use-package cider
  :ensure t
  :defer t
  :config
  (require 'general)

  (defun cider-system-reset ()
    "Call (user/reset)."
    (interactive)
    (save-excursion
      (cider-switch-to-repl-buffer)
      (goto-char cider-repl-input-start-mark)
      (delete-region (point) (point-max))
      (insert "(user/reset)")
      (cider-repl--send-input t)))

  (general-define-key
   :keymaps 'cider-mode-map
   "C-c r" 'cider-system-reset))

(use-package cljsbuild-mode
  :ensure t)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :ensure t)

(use-package sicp

  :ensure t)

(use-package geiser
  :straight t
  :init
  (setq geiser-active-implementations '(chicken guile)))

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :init
  (dolist (mode my/lisp-mode-hooks)
    (add-hook mode #'enable-paredit-mode)))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c RET"))

(add-hook 'clojure-mode-hook #'yas-minor-mode)

(use-package extempore-mode
  :ensure t)

(defvar keyword-lambda
  '(("(\\(lambda\\)\\>"
     (0 (prog1 () (compose-region
                   (match-beginning 1)
                   (match-end 1) ?λ))))))
(font-lock-add-keywords 'emacs-lisp-mode keyword-lambda)
(font-lock-add-keywords 'lisp-mode keyword-lambda)

;; Indenting module body code at column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(put 'module 'scheme-indent-function 'scheme-module-indent)

(put 'and-let* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode)))

(use-package pyvenv
  :ensure t
  :config
  (setq eshell-modify-global-environment t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (setq eshell-path-env (getenv "PATH"))))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda ()
                                          (setq eshell-path-env (getenv "PATH"))))
  )



(use-package python-black
  :straight t
  :after python
  :hook ((python-mode . python-black-on-save-mode)
         (python-ts-mode . python-black-on-save-mode)))

(flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                              '("pyproject.toml" "ruff.toml" ".ruff.toml"))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://beta.ruff.rs/docs/'."
  :command ("ruff"
            "check"
            (config-file "--config" flycheck-python-ruff-config)
            "--format=text"
            "--stdin-filename" source-original
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))


(setq-default flycheck-checkers
                  (append flycheck-checkers
                          '(python-ruff)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure t)

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode))

(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'" . coffee-mode))



(use-package js-comint
  :defer t
  :ensure t)

(setq js-indent-level 2)

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :ensure t)

(use-package js2-mode
  :straight t
  :hook ((js-mode . js2-minor-mode))
  :config
  (setf js2-mode-show-parse-errors nil)
  (setf js2-strict-missing-semi-warning nil))

(use-package mocha
  :straight t
  :general
  (general-define-key
   "C-c t ." 'mocha-test-at-point
   "C-c t f" 'mocha-test-file
   "C-c t p" 'mocha-test-project
   "C-c t r" 'recompile) 
  :config
  (setf mocha-environment-variables "FORCE_COLOR=1 NODE_ENV=test")
  (setf mocha-reporter "spec"))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode . add-node-modules-path)
         (solidity-mode . add-node-modules-path)))

(use-package nvm
  :straight t
  :hook ((js-mode . nvm-use-for)
         (solidity-mode . nvm-use-for)))

 (defun js2-imenu-make-index ()
    (save-excursion
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)

                                ;;add more keyword for mocha here
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)

                               ))))

 (add-hook 'js2-minor-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'js2-imenu-make-index)))

(defcustom prepend-mocha-generate-command ""
  "Prepend the mocha command with this string. Useful for running compilation step before tests."
  :safe #'stringp)

(defun wrap-mocha-generate-command (fn &rest args)
  (let ((cmd (apply fn args)))
    (concat prepend-mocha-generate-command cmd)))

(advice-add 'mocha-generate-command :around #'wrap-mocha-generate-command)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :defer t
  :diminish docker-mode)

(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
(setenv "DOCKER_CERT_PATH" "/Users/mark/.docker/machine/machines/default")
(setenv "DOCKER_MACHINE_NAME" "default")

(use-package eshell
  :commands (eshell)
  :config
  (defun esh-customize-faces ()
    (set-face-attribute 'eshell-ls-directory
                        nil
                        :foreground (plist-get my/base16-colors :base0C)
                        :background (plist-get my/base16-colors :base00)))

  (defmacro esh-section (name form &rest props)
    `(setq ,name
           (lambda ()
             (when ,form
               (-> ,form
                   (propertize 'face (list ,@props)))))))

  (defun esh-acc (acc x)
    (if-let ((section (funcall x)))
        (if (string-empty-p acc)
            section
          (concat acc esh-sep section))
      acc))

  (defun esh-prompt-func ()
    (concat
     (reduce #'esh-acc esh-funcs :initial-value "")
     ;; Reset face to default for input
     (propertize " " 'face 'default)))

  (esh-section esh-header
               "λ"
               `(:foreground ,(plist-get my/base16-colors :base08)))

  (esh-section esh-user
               (user-login-name)
               `(:foreground ,(plist-get my/base16-colors :base0B)))

  (esh-section esh-dir
               (concat "[" (abbreviate-file-name (eshell/pwd)) "]")
               `(:foreground ,(plist-get my/base16-colors :base0E)))

  (esh-section esh-git
               (when-let ((branch (magit-get-current-branch))) 
                 (concat " " branch))
               `(:foreground ,(plist-get my/base16-colors :base0D)))

  (esh-section esh-footer
               "\n→"
               `(:foreground ,(plist-get my/base16-colors :base0A)))

  (setq eshell-prompt-regexp "→ ")
  (setq eshell-skip-prompt-function #'eshell-skip-prompt)
  (setq esh-sep " ")
  (setq esh-funcs (list esh-header esh-user esh-dir esh-git esh-footer))

  (setq eshell-prompt-function 'esh-prompt-func)

  (add-hook 'eshell-mode-hook 'esh-customize-faces))

(use-package yaml-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

;; (use-package phpunit
;;   :load-path "~/.emacs.d/site-lisp/phpunit"
;;   :config
;;     (general-define-key
;;      :states '(normal)
;;      :keymaps 'php-mode-map
;;      :prefix "C-c"
;;      "C-t" 'phpunit-current-test))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

(use-package restclient
  :defer t
  :ensure t)

(use-package go-mode
  :ensure t
  :mode (("go\\.mod\\'" . go-mod-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :config)

(use-package gotest
  :defer t
  :ensure t)

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package seeing-is-believing
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'seeing-is-believing))

(use-package rbenv
    :ensure t
    :init
    (setq-default rbenv-installation-dir "/usr/local/Cellar/rbenv/1.1.2/")
    (defun my/ruby-init ()
      (rbenv-use-corresponding))
    (add-hook 'ruby-mode-hook 'my/ruby-init)
    :config
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)
    (rbenv-use-global)
)

(use-package rspec-mode
  :init
  (defun my/rspec-init ()
    (linum-mode -1)
    (local-set-key (kbd "r") 'inf-ruby-switch-from-compilation))
  (add-hook 'rspec-compilation-mode-hook 'my/rspec-init)
  :ensure t)

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package bundler
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.rbi$" . ruby-mode))

(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(use-package rust-mode
  :ensure t
  :after general
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  )

(use-package vimrc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tridactyl\\(rc\\)?\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package nix-mode
  :straight (nix-mode :type git :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'")

(use-package nix-sandbox
  :ensure t
  :config
  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args))))

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :straight (solidity-flycheck :type git :host github :repo "ethereum/emacs-solidity")
  :hook solidity-mode
  :config
  (setq solidity-flycheck-chaining-error-level t)
  (setq solidity-flycheck-use-project t))

(use-package company-solidity
  :straight (company-solidity :type git :host github :repo "ethereum/emacs-solidity")
  :defer t
  :init
  )

(use-package prettier
  :straight t
  :config)

(use-package vyper-mode
  :straight t)

(defun my/configure-org-directories ()
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-files (quote ("~/org")))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 4)))))

(require 'cl)

(defun org-open-other-frame ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-frame org-link-frame-setup)))
    (org-open-at-point)))

(setq org-log-into-drawer t)

(defun my/org-variable-pitch ()
  "Use variable pitch for prose text in org."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

(defun my/org-babel-config () 
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (plantuml . t)
     (shell . t))))

(defun my/configure-org ()
  (require 'org-tempo)
  (setq org-image-actual-width 300)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively nil)
  (setq org-fontify-done-headline nil)
  (setq org-log-done 'time)

  (setq org-src-window-setup 'current-window)

  (setq org-startup-truncated 'nil)
  (setq org-catch-invisible-edits 'smart)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Start up in org-indent-mode
  (setq org-startup-indented t)
  ;; Diminish org-indent-mode in the mode-line
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (setq org-hide-emphasis-markers t)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  (setq org-babel-python-command (concat (file-name-as-directory org-directory) "venv/bin/python"))

  (progn
    (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :height 1.25 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
    (set-face-attribute 'org-level-4 nil :height 1.1 :weight 'bold)
    (set-face-attribute 'org-level-5 nil :weight 'bold)
    (set-face-attribute 'org-level-6 nil :weight 'bold)
    (set-face-attribute 'org-level-7 nil :weight 'bold)
    (set-face-attribute 'org-level-8 nil :weight 'bold)

    (set-face-attribute 'org-special-keyword nil :foreground "gray25")
    (set-face-attribute 'org-date nil :foreground "gray25")

    (set-face-attribute 'org-drawer nil :foreground "grey25"))

  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun my/org-mode ()
  (visual-line-mode 1))

(setq-default fill-column 85)

(use-package org
  :straight org-contrib
  :config
  (require 'org-eldoc))

(use-package org-z
  :straight (org-z :type git :host github :repo "landakram/org-z")
  :general
  (leader-def :infix "o"
    "b" 'org-z-backlinks-at-point)
  :config
  (org-z-mode 1))

(use-package org-ql
  :straight t)

(use-package org-z-selectrum
  :straight (org-z-selectrum :type git :host github :repo "landakram/org-z"))

(general-define-key
 :states '(emacs)
 :keymaps 'org-agenda-mode-map
 :prefix ""
 "c" 'org-agenda-capture)

(defun org-insert-subheading-after-current ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote))

(defun org-insert-subheading-after-current-and-enter-insert ()
  (interactive)
  (org-insert-subheading-after-current)
  (evil-append 0))

(defun org-insert-subheading-and-enter-insert ()
  (interactive)
  (org-insert-subheading nil)
  (evil-append 0))

(defun org-insert-heading-after-current-and-enter-insert ()
  (interactive)
  (org-insert-heading-after-current)
  (evil-append 0))

(defun org-insert-heading-and-enter-insert ()
  (interactive)
  (org-insert-heading)
  (evil-append 0))

(defun org-insert-todo-after-current-and-enter-insert ()
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-append 0))

(defun my/configure-org-todos ()
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "MAYBE(m)" "REPEATING(r)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "DELEGATED(e)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-use-fast-todo-selection t))

(defun org-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function 'current-time)
              #'(lambda () my-current-time))
             ((symbol-function 'org-today)
              #'(lambda () (time-to-days my-current-time)))
             ((symbol-function 'org-current-effective-time)
              #'(lambda () my-current-time))
             (super (symbol-function 'format-time-string))
             ((symbol-function 'format-time-string)
              #'(lambda (fmt &optional time time-zone)
                  (funcall super fmt my-current-time time-zone))))
    (org-todo arg)))

(setq org-agenda-span 2)

(use-package org-super-agenda
  :straight t
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;; See https://github.com/alphapapa/org-super-agenda/issues/50
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (setq org-super-agenda-header-separator "")
  (setq org-super-agenda-unmatched-name "Scheduled")
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Habits"
                 :habit t)))
  )

(defun my/configure-org-exporters ()
  (use-package ox-gfm
    :ensure t)

  (use-package org-habit)

  (use-package ox-odt
    :config
    (setq org-odt-preferred-output-format "rtf"))

  (use-package ox-jira
    :ensure t)

  (use-package ox-rst
    :ensure t))

(defvar default-jira-repository)
(setq default-jira-repository "getclef.atlassian.net")

(defun org-jira-insert-link (issue-name description)
  "Add links to JIRA issues by title."
  (interactive "sIssue: \nsDescription: ")
  (let ((desc (if (string= "" description) issue-name description))) 
    (org-insert-link nil (concat "https://" default-jira-repository "/browse/" issue-name) desc)))

(use-package org-capture
  :general
  (leader-def
    "c" 'org-capture) 
  :config
  (setq org-tag-alist '((:startgroup . nil)
                        ("WORK" . ?w)
                        ("PERSONAL" . ?p)
                        ("ERRANDS" . ?e)
                        ("HABIT" . ?h)
                        (:endgroup . nil)
                        ("crypt" . ?c)
                        ("WATCH" . ?s)
                        ("READ" . ?r)))

  (setq org-capture-templates
        '(("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
           "* %?\n:LOGBOOK:\n- Added %U\n:END:\n%a\n")
          ("p" "Project idea" entry (file+headline "~/org/inbox.org" "Project Ideas")
           "* %?\n:LOGBOOK:\n- Added %U\n:END:\n%a\n")
          ("c" "Calendar" entry (file+olp+datetree "~/org/calendar.org" "Calendar")
           "* %?\n")
          ("j" "Journal entry" entry (file+datetree "~/org/journal.org")
           "* %<%H:%M>\n%?")
          ("m" "Email follow-up" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO Follow up with %:fromname\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:LOGBOOK:\n- Added %U\n:END:\n%a\n%?")
          ("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:LOGBOOK:\n- Added %U\n:END:\n%a\n"))))

(defun my/configure-org-protocol ()
  (use-package org-protocol)
  (server-start))

(use-package org
  :straight t
  :general
  (:states '(normal)
   :keymaps 'org-mode-map
   :prefix "SPC"
   "*" 'org-ctrl-c-star
   "a" 'org-agenda
   "ih" 'org-insert-heading-after-current-and-enter-insert
   "iH" 'org-insert-heading-and-enter-insert
   "is" 'org-insert-subheading-after-current-and-enter-insert
   "iS" 'org-insert-subheading-and-enter-insert
   "it" 'org-insert-todo-after-current-and-enter-insert
   "n" 'org-narrow-to-subtree
   "N" 'widen
   "ml" 'org-do-demote
   "mL" 'org-demote-subtree
   "mh" 'org-do-promote
   "mH" 'org-promote-subtree
   "mk" 'org-metaup
   "mj" 'org-metadown
   "s" 'org-schedule
   "t" 'org-todo)
  :config

  (my/configure-org-directories)
  (my/configure-org-exporters)
  (my/configure-org-todos)
  (my/configure-org)
  (my/configure-org-protocol)
  (my/org-babel-config)
  (setq org-inline-image-overlays t)

  (add-hook 'org-mode-hook #'my/org-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-archive
  :bind (:map org-mode-map
              ("C-c $" . org-archive-subtree))
  :config
  (defun org-archive-subtree-hierarchical--line-content-as-string ()
    "Returns the content of the current line as a string"
    (save-excursion
      (beginning-of-line)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position))))

  (defun org-archive-subtree-hierarchical--org-child-list ()
    "This function returns all children of a heading as a list. "
    (interactive)
    (save-excursion
      ;; this only works with org-version > 8.0, since in previous
      ;; org-mode versions the function (org-outline-level) returns
      ;; gargabe when the point is not on a heading.
      (if (= (org-outline-level) 0)
          (outline-next-visible-heading 1)
        (org-goto-first-child))
      (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
        (while (org-goto-sibling)
          (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
        child-list)))

  (defun org-archive-subtree-hierarchical--org-struct-subtree ()
    "This function returns the tree structure in which a subtree
belongs as a list."
    (interactive)
    (let ((archive-tree nil))
      (save-excursion
        (while (org-up-heading-safe)
          (let ((heading
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
            (if (eq archive-tree nil)
                (setq archive-tree (list heading))
              (setq archive-tree (cons heading archive-tree))))))
      archive-tree))

  (defun org-archive-subtree-hierarchical ()
    "This function archives a subtree hierarchical"
    (interactive)
    (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
          (this-buffer (current-buffer))
          (file (abbreviate-file-name
                 (or (buffer-file-name (buffer-base-buffer))
                     (error "No file associated to buffer")))))
      (save-excursion
        (setq location org-archive-location
              afile (car (org-archive--compute-location
                          (or (org-entry-get nil "ARCHIVE" 'inherit) location)))
              ;; heading (org-extract-archive-heading location)
              infile-p (equal file (abbreviate-file-name (or afile ""))))
        (unless afile
          (error "Invalid `org-archive-location'"))
        (if (> (length afile) 0)
            (setq newfile-p (not (file-exists-p afile))
                  visiting (find-buffer-visiting afile)
                  buffer (or visiting (find-file-noselect afile)))
          (setq buffer (current-buffer)))
        (unless buffer
          (error "Cannot access file \"%s\"" afile))
        (org-cut-subtree)
        (set-buffer buffer)
        (org-mode)
        (goto-char (point-min))
        (while (not (equal org-tree nil))
          (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
            (if (member (car org-tree) child-list)
                (progn
                  (search-forward (car org-tree) nil t)
                  (setq org-tree (cdr org-tree)))
              (progn
                (goto-char (point-max))
                (newline)
                (org-insert-struct org-tree)
                (setq org-tree nil)))))
        (newline)
        (org-yank)
        (when (not (eq this-buffer buffer))
          (save-buffer))
        (message "Subtree archived %s"
                 (concat "in file: " (abbreviate-file-name afile))))))

  (defun org-insert-struct (struct)
    "TODO"
    (interactive)
    (when struct
      (insert (car struct))
      (newline)
      (org-insert-struct (cdr struct))))

  (defun org-archive-subtree ()
    (interactive)
    (org-archive-subtree-hierarchical)
    )
  )

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path (expand-file-name "~/src/plantuml.jar"))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (add-hook 'org-mode-hook (lambda () 
                             (add-to-list
                              'org-src-lang-modes '("plantuml" . plantuml)))))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(setq my-credentials-file "~/.private.el")

(defun my/nickserv-password (_)
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :nickserv-password)))

(use-package circe
  :ensure t
  :defer t
  :config

  (setq circe-network-options
        `(("Freenode"
           :nick "landakram"
           :channels (:after-auth
                      "#emacs"
                      "#clojure"
                      "#clojure-beginners"
                      "#iphonedev"
                      "#swift-lang"
                      "#racket"
                      "#chicken"
                      "#lisp"
                      "#stumpwm"
                      "#archlinux"
                      "#ethereum"
                      "#ethereum-dev"
                      "#bitcoin"
                      "#bitcoin-core-dev"
                      "#ipfs"
                      "#n-o-d-e"
                      "#ruby")
           :nickserv-password ,(my/nickserv-password nil)
           :reduce-lurker-spam t)))
  (enable-circe-color-nicks))

(use-package elfeed
  :ensure t
  :defer t
  :config

  (setq elfeed-feeds
        '("http://lambda-the-ultimate.org/rss.xml"
          "http://planet.emacsen.org/atom.xml"
          "http://www.overcomingbias.com/feed"
          "http://slatestarcodex.com/feed/"
          "http://worrydream.com/feed.xml"
          "https://xkcd.com/rss.xml"
          "http://existentialcomics.com/rss.xml"
          "http://joshldavis.com/atom.xml"
          "https://rationalconspiracy.com/feed/"
          "https://soylentnews.org/index.rss"
          "http://meaningness.com/rss.xml"
          "http://feeds.ribbonfarm.com/Ribbonfarm"
          "http://www.cs.uni.edu/~wallingf/blog/index.xml"
          ;;"https://feeds.feedburner.com/Metafilter"
          "http://feeds.feedburner.com/thoughtsfromtheredplanet?format=xml"
          "http://www.gwern.net/atom.xml"
          "http://airspeedvelocity.net/feed/")))

(setq url-queue-timeout 30)

(setq mm-sign-option 'guided)

  (use-package mu4e
    :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"
    :commands (mu4e)
    :after general
    :config
    ;; Sync every 10 minutes
    (setq mu4e-update-interval (* 60 10))
    (setq mu4e-maildir (expand-file-name "~/Maildir"))
    (setq mu4e-compose-format-flowed t)

    ;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
    ;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    ;; (setq mu4e-trash-folder  "/[Gmail].Trash")

    ;; don't save message to Sent Messages, GMail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; (setq mu4e-maildir-shortcuts
    ;;       '(("/INBOX"             . ?i)
    ;;         ("/[Gmail].Sent Mail" . ?s)
    ;;         ("/[Gmail].Trash"     . ?t)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "mbsync -a")

    (setq mu4e-change-filenames-when-moving t)

    ;; Show images
    (setq mu4e-view-show-images t)

    ;; Don't use mu4e's default HTML renderer. It's hard to read for most messages.
    (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72")
    ;; Ignore mu4e's plaintext heuristic.
    ;; See https://200ok.ch/posts/2018-10-25_disable_mu4e_html_over_plain_text_heuristic.html
    (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

    (setq mu4e-view-show-addresses t)

    (setq browse-url-generic-program 'browse-url-default-browser)

    ;; 
    (add-to-list 'mu4e-view-actions
                 '("open URL" . mu4e-view-go-to-url) t)

    (add-to-list 'mu4e-view-actions
                 '("browser (open in)" . mu4e-action-view-in-browser) t)

    (general-define-key :keymaps '(mu4e-view-mode-map)
                        "J" 'mu4e-view-headers-next
                        "K" 'mu4e-view-headers-prev)

    (setq
     user-mail-address "me@markhudnall.com"
     user-full-name  "Mark Hudnall"
     ;; message-signature
     ;;  (concat
     ;;    "Foo X. Bar\n"
     ;;    "http://www.example.com\n")
     )

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

    (use-package smtpmail
      :ensure t
      :config
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials
            '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials
            (expand-file-name "~/.authinfo.gpg")
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587
            smtpmail-debug-info t))

    (use-package org-mu4e
      :config)

    (setq mu4e-tags '(
                      "jobs"
                      ))

    (add-to-list 'mu4e-marks
                 '(tagarchive
                   :char       "t"
                   :prompt     "tagarchive"
                   :ask-target (lambda () (completing-read "Choose a tag: " mu4e-tags))
                   :action      (lambda (docid msg target)
                                  (mu4e-action-retag-message msg (concat "+" target ",-\\Inbox"))
                                  (mu4e~proc-move docid mu4e-refile-folder))))

    (add-to-list 'mu4e-marks
                 '(tag
                   :char       "T"
                   :prompt     "tag"
                   :ask-target (lambda () (completing-read "Choose a tag: " mu4e-tags))
                   :action      (lambda (docid msg target)
                                  (mu4e-action-retag-message msg (concat "+" target)))))


    (defun mu4e-headers-mark-for-tag (args)
      "Mark header at point with tag."
      (interactive "P")
      (if args
          (mu4e-headers-mark-and-next 'tag)
          (mu4e-headers-mark-and-next 'tagarchive)))
    (general-define-key :keymaps '(mu4e-headers-mode-map)
                        "t" 'mu4e-headers-mark-for-tag)

    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Personal"
               :enter-func (lambda () (mu4e-message "Switch to the Personal context"))
               ;; leave-func not defined
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "me@markhudnall.com")))
               :vars '((user-mail-address        . "me@markhudnall.com"  )
                       (user-full-name           . "Mark Hudnall" )
                       (mu4e-compose-reply-to-address           . "me@markhudnall.com" )
                       (mu4e-drafts-folder       . "/Personal/[Gmail]/.Drafts")
                       (mu4e-sent-folder         . "/Personal/[Gmail]/.Sent Mail")
                       (mu4e-trash-folder        . "/Personal/[Gmail]/.Trash")
                       (mu4e-refile-folder       . "/Personal/[Gmail]/.All Mail")
                       (mu4e-maildir-shortcuts   . (("/Personal/INBOX" . ?i)
                                                    ("/Personal/[Gmail]/.Sent Mail"  . ?s)
                                                    ("/Personal/[Gmail]/.All Mail"  . ?a)
                                                    ("/Personal/[Gmail]/.Trash" . ?t)))
                       (mu4e-compose-signature   . nil)))))

    ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
    ;; guess or ask the correct context, e.g.

    ;; start with the first (default) context; 
    ;; default is to ask-if-none (ask when there's no context yet, and none match)
    ;; (setq mu4e-context-policy 'pick-first)

    ;; compose with the current context is no context matches;
    ;; default is to ask 
    ;; '(setq mu4e-compose-context-policy nil)

    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
)

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :ensure t)

(use-package elpher
  :straight t
  :after (general)
  :defer t
  :config
  (evil-set-initial-state 'elpher-mode 'motion)
  (general-define-key
   :keymaps '(elpher-mode-map)
   "C-j" 'evil-scroll-down
   "C-k" 'evil-scroll-up))

(defun shell-command-ignore-stderr (some-command)
  (with-output-to-string
    (with-current-buffer standard-output
      (process-file shell-file-name nil '(t nil)  nil shell-command-switch some-command))))

(use-package request
  :ensure t)

(use-package request-deferred
  :ensure t)

(use-package deferred
  :ensure t)

(use-package concurrent
  :ensure t)

(use-package s
  :ensure t)

(defun snake-case-thing-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (thing (buffer-substring-no-properties start end))
         (new-thing (s-snake-case
                     (s-lower-camel-case thing))))
    (delete-region start end)
    (insert new-thing)))

(defun camel-case-thing-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (thing (buffer-substring-no-properties start end))
         (new-thing (s-lower-camel-case thing)))
    (delete-region start end)
    (insert new-thing)))

(use-package dash
  :ensure t)

(use-package ht
  :ensure t)

(use-package parse-csv
  :straight (parse-csv :type git :host github :repo "mrc/el-csv")
  :config
  (defun csv-row-get (header row key)
    (let ((index (-find-index (-partial #'equal key) header)))
      (nth index row)))

  (defun venmo-to-ynab-row (header row)
    (let* ((memo (->> (or (csv-row-get header row "Note") "")
                      (s-collapse-whitespace)))
           (date (ignore-errors
                   (--> (csv-row-get header row "Datetime")
                        (parse-iso8601-time-string it))))
           (ynab-date (and (car date) (format-time-string "%m/%d/%Y" date)))
           (amount (-some->> (csv-row-get header row "Amount (total)")
                     (s-replace " $" "")
                     (s-replace "," "")
                     (string-to-number)))
           (inflow (when (and amount (>= amount 0))
                     (format "%0.2f" (abs amount))))
           (outflow (when (and amount (< amount 0))
                      (format "%0.2f" (abs amount))))
           (payee (if inflow
                      (csv-row-get header row "From")
                    (csv-row-get header row "To"))))
      (when (and (or inflow outflow)
                 ynab-date)
        (list ynab-date payee memo outflow inflow))))

  (defun venmo-to-ynab ()
    (interactive)
    (let* ((venmo-statement-filename (read-file-name
                                      "Venmo statement: "
                                      "~/Downloads/venmo_statement.csv"))
           (venmo-statement (--> venmo-statement-filename
                                 (with-temp-buffer
                                   (insert-file-contents it)
                                   (buffer-string))
                                 (parse-csv-string-rows it  ?\, ?\" "\n")))
           (header (car venmo-statement))
           (body (cdr venmo-statement))
           (new-body 
            (->> body
                 (mapcar (-partial #'venmo-to-ynab-row header))
                 (remove nil)))
           (new-header '("Date" "Payee" "Memo" "Outflow" "Inflow"))
           (ynab-statement-filename "~/Downloads/venmo_ynab_statement.csv"))
      (with-temp-file ynab-statement-filename
        (insert (s-join "," new-header) "\n")
        (dolist (row new-body)
          (insert (s-join "," row) "\n")))

      (let ((default-directory "~/Downloads"))
        (shell-command "open -R .")))))

(electric-pair-mode t)
