(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; (add-to-list 'default-frame-alist
;;              '(font . "Fira Mono-11"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

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

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(display-time-mode 1)

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'base16-shell)
  (setq base16-distinct-fringe-background nil)
  (load-theme 'base16-tomorrow-night t))

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
                          ruby-rubocop
                          ruby-reek
                          emacs-lisp-checkdoc))))

(setq ring-bell-function 'ignore)

(use-package better-defaults
  :ensure t)

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
  (setq which-key-idle-delay 0.5))

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
  :hook (after-init . session-initialize))

(ido-mode -1)

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
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :defer .1
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
    :ensure t
    :diminish evil-surround-mode
    :config
    (global-evil-surround-mode 1))

  (use-package evil-magit
    :straight (evil-magit :type git :host github :repo "emacs-evil/evil-magit"))

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1)))

  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

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
    "o" '(:which-key "org-file"
                     :def (lambda () (interactive) (find-file "~/org/projects.org"))))

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

  (defun lsp-find-definition-with-fallback ()
    (interactive)
    (let ((definition (lsp-find-definition)))
      (when (or (string-prefix-p "Not found for:" definition))
        (dumb-jump-go))))

  ;; JavaScript
  (general-define-key :keymaps '(typescript-mode-map javascript-mode-map)
                      "gf" 'lsp-find-definition-with-fallback)

  ;; Ruby
  (general-define-key :keymaps '(ruby-mode-map)
                      "gf" 'lsp-find-definition-with-fallback)

  ;; Use go-specific jumping for go-mode since it works wells
  (general-define-key :keymaps 'go-mode-map
                      "gf" 'godef-jump)

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
   "f" 'avy-goto-word-or-subword-1)

  (general-define-key
   :states '(normal)
   "gf" (lambda () 
          (interactive)
          (if current-prefix-arg
              (dumb-jump-go-other-window)
            (dumb-jump-go)))
   "gb" 'dumb-jump-back))

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
  :ensure t
  :commands (dumb-jump-go dumb-jump-go-back dumb-jump-go-other-window)
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

(setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")

(use-package pdf-tools
  :ensure t
  :after evil
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ;; Use brew upgrade pdf-tools instead
  (pdf-tools-install :no-query)

  (setq-default image-mode-winprops-alist nil)

  (leader-def :infix ","
    :keymaps 'pdf-view-mode-map
    "h" 'pdf-annot-add-highlight-markup-annotation
    "m" 'pdf-annot-add-annotation)

  (add-hook 'pdf-annot-edit-contents-minor-mode-hook (lambda ()
                                                       (org-mode)
                                                       ))
  )

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package org-noter
  :defer t
  :ensure t)

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :ensure t
  :after (org-noter)
  :commands org-noter-pdftools-jump-to-note
  :init
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-created-annotations #'org-noter-pdftools-jump-to-note)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq help-window-select t)

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;; (use-package helm
;;   :ensure t
;;   :defer t
;;   :after (general projectile)
;;   :diminish helm-mode
;;   :config
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (global-set-key (kbd "M-x") 'helm-M-x)
;;   (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;   (global-set-key (kbd "C-x b") 'helm-mini)
;;   (global-set-key (kbd "C-h a") 'helm-apropos)
;;   (setq helm-buffer-max-length nil)
;;   (add-to-list 'completion-styles 'helm)
;;   (helm-autoresize-mode t)

;;   (require 'helm-imenu)

;;   (leader-def :infix "b"
;;     "b" 'helm-mini)

;;   (leader-def
;;     "x" 'helm-M-x)

;;   (leader-def :infix "f"
;;     "f" 'helm-find-files)

;;   (leader-def :infix "p"
;;     "a" 'helm-do-ag-project-root)

;;   (def-projectile-commander-method ?a
;;     "Full text search in the project."
;;     (helm-do-ag-project-root))

;;   (general-define-key :states '(normal)
;;                       "F" 'helm-semantic-or-imenu)

;;   (add-hook 'eshell-mode-hook
;;             (lambda()
;;               (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
;;               (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;;               (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))))

;; (use-package helm-ag
;;   :ensure t
;;   :config
;;   (setq helm-ag-base-command "rg --no-heading --smart-case --hidden"))

;; (use-package helm-rg
;;   :ensure t)

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))

(use-package selectrum
  :straight (selectrum :type git :host github :repo "raxod502/selectrum")
  :config

  (plist-get base16-shell-colors-256 :base09)
  (set-face-attribute 'selectrum-current-candidate nil
                      :foreground (plist-get base16-tomorrow-night-colors :base09)
                      :background (plist-get base16-tomorrow-night-colors :base01))
  (set-face-attribute 'selectrum-primary-highlight nil
                      :foreground (plist-get base16-tomorrow-night-colors :base0E))
  (set-face-attribute 'selectrum-secondary-highlight nil
                      :foreground (plist-get base16-tomorrow-night-colors :base0D))

  (selectrum-mode +1)

  (leader-def :infix "f"
    "f" 'find-file)

  (leader-def 
    "x" 'execute-extended-command))

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

  (leader-def :infix "b"
    "b" 'consult-buffer)

  (general-define-key :states '(normal)
                      "F" 'consult-outline)

  (leader-def :infix "p"
    "a" 'consult-ripgrep)

  (def-projectile-commander-method ?a
    "Full text search in the project."
    (consult-ripgrep))

  (add-hook 'eshell-mode-hook
            (lambda()
              (define-key eshell-mode-map (kbd "M-r") 'consult-history))))

(use-package consult-selectrum
  :straight (consult-selectrum :type git :host github :repo "minad/consult" :branch "main")
  :after selectrum
  :demand t)

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :straight (consult-flycheck :type git :host github :repo "minad/consult" :branch "main")
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package selectrum-prescient
  :straight (selectrum-prescient :type git :host github :repo "raxod502/prescient.el")
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :bind
  (:map selectrum-minibuffer-map
        ("C-j" . embark-act))

  :config
  (setq embark-prompter 'embark-completing-read-prompter))

(use-package marginalia
  :straight (marginalia :type git :host github :branch "main" :repo "minad/marginalia")
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

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

(use-package tdd
  :load-path "site-lisp/tdd/")

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode))

(use-package company
  :ensure t
  :defer 0.1
  :config
  (setq company-idle-delay 0) 
  (setq company-minimum-prefix-length 1)
  (global-company-mode)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  ;; Certain helm-mode functions rely on 'helm being in completion-styles to
  ;; be able to use helm-style searching with e.g. helm-M-x.
  ;;
  ;; But that also causes completion-at-point using capf to be really slow,
  ;; since it does some non-prefix matching over really big lists.
  ;; To get around that, pin completion styles to remove 'helm when doing
  ;; company-capf
  (defun pin-completion-styles (orig-fn &rest args)
    (let ((completion-styles (remove 'helm completion-styles)))
      (apply orig-fn args)))

  (advice-add 'company-capf :around #'pin-completion-styles))

(use-package magit
  :ensure t
  :defer t
  :config
  ;; Uncomment this to improve performance
  ;; (setq magit-refresh-status-buffer nil)
  ;; (setq magit-refresh-verbose t)
  )

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
  :config)

(use-package cljsbuild-mode
  :ensure t)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :ensure t)

(use-package sicp

  :ensure t)

(use-package geiser
  :ensure t
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

;; Indenting module body code at column 0
(defun scheme-module-indent (state indent-point normal-indent) 0)
(put 'module 'scheme-indent-function 'scheme-module-indent)

(put 'and-let* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

(use-package pyvenv
  :ensure t
  :config
  (setq eshell-modify-global-environment t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (setq eshell-path-env (getenv "PATH"))))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda ()
                                          (setq eshell-path-env (getenv "PATH"))))
  )

(use-package elpy
  :ensure t
  :hook (python-mode . elpy-enable)
  :config
  ;; Set pytest as the default test runner
  (elpy-set-test-runner 'elpy-test-pytest-runner)

  ;; Temporary workaround for a warning that comes up: https://github.com/jorgenschaefer/elpy/issues/887
  (setq python-shell-completion-native-enable nil))

(use-package py-yapf
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

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

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp))
  :init
  (add-hook 'haskell-literate-mode-abbrev-table #'lsp)
  (setq lsp-haskell-server-wrapper-function
        (lambda (argv)
          (append
           (append (list "nix-shell" "-I" "." "--command" )
                   (list (mapconcat 'identity argv " "))
                   )
           (list (nix-current-sandbox))))))

(use-package docker
  :ensure t
  :defer t
  :diminish docker-mode)

(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
(setenv "DOCKER_CERT_PATH" "/Users/mark/.docker/machine/machines/default")
(setenv "DOCKER_MACHINE_NAME" "default")

(setq shell-file-name "/bin/zsh")

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
  :mode (("go\\.mod\\'" . go-dot-mod-mode)
         ("\\.go\\'" . go-mode))
  :config
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

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
  (global-rbenv-mode)
  (rbenv-use-global))

(use-package rspec-mode
  :init
  (defun my/rspec-init ()
    (setq compilation-scroll-output t)
    (linum-mode -1)
    (local-set-key (kbd "r") 'inf-ruby-switch-from-compilation))
  (add-hook 'rspec-compilation-mode-hook 'my/rspec-init)
  :ensure t)

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package bundler
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.rbi$" . ruby-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (ruby-mode . lsp-deferred))
  :config
  (require 'lsp-go)
  (require 'lsp-solargraph)
  (customize-set-variable 'lsp-solargraph-use-bundler nil)
  (customize-set-variable 'lsp-solargraph-multi-root nil)

  (setq lsp-log-io nil)
  (setq lsp-clients-typescript-server-args '("--stdio")))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-prefer-capf t)

(use-package rust-mode
  :ensure t
  :after general
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (general-define-key :keymaps '(rust-mode-map)
                      "gf" 'lsp-find-definition))

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
  :defer t
  :init
  (setq solidity-flycheck-solc-checker-active t)
  (add-hook 'solidity-mode-hook (lambda ()
                                  (require 'solidity-flycheck))))

(use-package company-solidity
  :ensure t
  :defer t
  :init
  (add-hook 'solidity-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-solidity company-capf company-dabbrev-code))
                           company-backends)))))

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

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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

(use-package org-z
  :defer t
  :straight (org-z :type git :host github :repo "landakram/org-z"))

(use-package org-z-selectrum
  :general
  ("C-c C-." 'org-z-insert-link)
  :straight (org-z :type git :host github :repo "landakram/org-z")
  :config
  (setq org-z-knowledge-dirs (-concat org-z-directories
                                      '("/Users/mark/Dropbox (Personal)/Apps/KiwiApp/wiki/")))

  (setq org-z-refile-missing-heading nil)
  (org-z-mode 1))

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

(setq org-agenda-span 2)

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
           "* %?\n" :unnarrowed t)
          ("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:LOGBOOK:\n- Added %U\n:END:\n%a\n"))))

(defun my/configure-org-protocol ()
  (use-package org-protocol)
  (server-start))

(use-package org
  :ensure org-plus-contrib
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

(use-package org-ql
  :ensure t
  :defer t)

(use-package org-sidebar
  :ensure t
  :general
  (leader-def :infix "o"
    "b" 'org-sidebar-backlinks)
  :after (general)
  :config
  ;; Work around https://github.com/alphapapa/org-sidebar/issues/32
  (require 'org-ql-search))

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
    :defer t
    :after general
    :config
    ;; default
    (setq mu4e-maildir (expand-file-name "~/Maildir"))

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
    ;; (setq mu4e-html2text-command 'mu4e-shr2text)
    (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72")

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
                                                    ("Personal/[Gmail]/.Sent Mail"  . ?s)
                                                    ("Personal/[Gmail]/.Trash" . ?t)))
                       (mu4e-compose-signature   . nil)))))

    ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
    ;; guess or ask the correct context, e.g.

    ;; start with the first (default) context; 
    ;; default is to ask-if-none (ask when there's no context yet, and none match)
    ;; (setq mu4e-context-policy 'pick-first)

    ;; compose with the current context is no context matches;
    ;; default is to ask 
    ;; '(setq mu4e-compose-context-policy nil)

    ;; (setq mu4e-update-interval 300)
)

(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :ensure t)

(use-package prodigy
  :ensure t)

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

(use-package esup
  :straight t
  :config
  (setq esup-user-init-file (file-truename "~/.emacs.d/config.el")))
