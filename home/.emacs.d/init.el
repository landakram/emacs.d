(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq frame-resize-pixelwise t))

(setq native-comp-async-report-warnings-errors nil)

(setq warning-minimum-level :error)

(defun eval-after-load-all (my-features form)
  "Run FORM after all MY-FEATURES are loaded.
See `eval-after-load' for the possible formats of FORM."
  (if (null my-features)
      (if (functionp form)
          (funcall form)
        (eval form))
    (eval-after-load (car my-features)
      `(lambda ()
         (eval-after-load-all
          (quote ,(cdr my-features))
          (quote ,form))))))

(defmacro with-eval-after-loads (my-features &rest body)
  (declare (indent 1) (debug (form def-body)))
  `(eval-after-load-all ,my-features (lambda () ,@body)))

(defun image-to-base64-data-url (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     (concat "data:" mime-type ";base64," base64-string)))

(defun image-to-base64 (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     base64-string))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
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

;; These should be loaded early on
(straight-use-package '(org :type built-in))

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# This buffer is for notes you don't want to save.")

(defvar use-package-config-hooks nil
  "Alist of hooks to run after a use-package config block.")

(defun add-use-package-config-hook (package hook)
  "Add a function to the list of hooks to be run after PACKAGE's use-package config."
  (let ((entry (assoc package use-package-config-hooks)))
    (if entry
        (push hook (cdr entry))
      (setq use-package-config-hooks
            (acons package (list hook) use-package-config-hooks)))))

(defun run-use-package-config-hooks (package)
  "Run all hooks for the given PACKAGE."
  (dolist (hook (cdr (assoc package use-package-config-hooks)))
    (funcall hook)))

(defmacro with-eval-after-use-package-config (package &rest body)
  "Specify actions to run after PACKAGE is configured via use-package."
  `(add-use-package-config-hook ,package (lambda () ,@body)))

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
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format mood-line-format-default)
  (mood-line-mode))

(use-package catppuccin-theme
  :straight t
  :config
  (setq catppuccin-flavor 'mocha)
  ;; Slightly darker background
  (catppuccin-set-color 'base "#15151f")
  (load-theme 'catppuccin :no-confirm)
  )

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'base16-shell)
  (setq base16-distinct-fringe-background nil))

(defvar my/base16-color-alist
  '((rosewater . nil)
    (flamingo  . nil)
    (pink      . nil)
    (mauve     . :base0E)
    (red       . :base08)
    (maroon    . :base0F)
    (peach     . :base09)
    (yellow    . :base0A)
    (green     . :base0B)
    (teal      . nil)
    (sky       . :base0C)
    (sapphire  . nil)
    (blue      . :base0D)
    (lavender  . :base07)
    (text      . :base06)
    (subtext1  . :base04)
    (overlay2  . :base03)
    (overlay1  . :base02)
    (overlay0  . :base01)
    (surface2  . :base03)
    (surface1  . :base02)
    (surface0  . :base00)
    (base      . :base00)
    (mantle    . :base00)
    (crust     . :base00)))

(with-eval-after-loads '(catppuccin-theme base16-theme)
  (defun my/base16-color-get (name)
    "Lookup NAME (e.g. 'blue) from base16 using base16-color-alist."
    (let* ((base16-key (alist-get name my/base16-color-alist)))
      (unless base16-key
        (error "Base16 color '%s' not mapped to a base key" name))
      (or (plist-get base16-theme-shell-colors-256 base16-key)
          (error "Base16 key %s not found in my/base16-colors" base16-key))))

  (defun my/catppuccin-color-get (name)
    "Lookup NAME (e.g. 'blue) from catppuccin color alist."
    (or (cdr (assoc name catppuccin-mocha-colors))
        (error "Catppuccin color '%s' not found" name)))

  (defun my/theme-color-get (name)
    "Call the active theme color getter with NAME."
    (my/catppuccin-color-get name)))

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
  (message "Attempting to load host-specific config file %s" host-specific-config)
  (when (file-readable-p host-specific-config)
    (message "Found host-specific config file %s. Loading." host-specific-config)
    (load-file host-specific-config)))

(use-package session
  :ensure t
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file-coding-system 'utf-8))

(ido-mode -1)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq jit-lock-defer-time 0.05)

(use-package gcmh
  :straight t
  :config
  (setq gcmh-high-cons-threshold (* 1024 1024 1024))
  (setq gcmh-idle-delay-factor 20)
  (gcmh-mode 1))

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
    "k" 'kill-current-buffer
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
   "f" 'avy-goto-word-or-subword-1)

  (run-use-package-config-hooks 'general))

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

  (setq evil-want-minibuffer t)

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

(use-package evil-textobj-tree-sitter :after evil :straight t
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-inner-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj ("block.inner"))) 
  (define-key evil-outer-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj ("block.outer")))

  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))

  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner")))

(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :init
  ;; Optional: turn on subword-mode for better word selection in camelCase
  (add-hook 'prog-mode-hook #'subword-mode)

  :config
  ;; Leader-based bindings (adjust prefix as needed)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC e"
   "e" '(expreg-expand :which-key "expand region")
   "r" '(expreg-contract :which-key "contract region"))

  ;; Optional: ergonomic bindings in visual mode
  (general-define-key
   :states '(visual)
   "v" #'expreg-expand
   "V" #'expreg-contract))


(use-package evil-ts :straight (evil-ts :type git :host github :repo "foxfriday/evil-ts"))

(use-package jumpy
  :demand t
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

(use-package project)

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

(use-package link-hint
  :straight t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)
  :general
  (leader-def :infix "l"
    "o" 'link-hint-open-link
    "c" 'link-hint-copy-link)
)

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
Result is full path or relative path if a root is specified.
If `universal-argument' is called first, prompt for root and copy the relative path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2024-06-06"
  (interactive "P")
  (let* ((root-path (if *dir-path-only-p
                        (read-directory-name "Select root: ")
                      nil))
         (-fpath (if (equal major-mode 'dired-mode)
                     (expand-file-name default-directory)
                   (if (buffer-file-name)
                       (buffer-file-name)
                     (user-error "Current buffer is not associated with a file."))))
         (result-path (if root-path
                          (file-relative-name -fpath root-path)
                        -fpath)))
    (kill-new result-path)
    (message "Path copied: %s" result-path)))

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
   "l" 'dired-find-file
   "/" 'find-file)
  :config
  (setq dired-listing-switches "-alh"))

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode)

  (set-face-attribute 'diredfl-dir-priv nil
                      :foreground (my/theme-color-get 'blue)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-read-priv nil
                      :foreground (my/theme-color-get 'green)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-write-priv nil
                      :foreground (my/theme-color-get 'yellow)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-exec-priv nil
                      :foreground (my/theme-color-get 'red)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-no-priv nil
                      :foreground (my/theme-color-get 'overlay2)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-dir-name nil
                      :foreground (my/theme-color-get 'sky)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-symlink nil
                      :foreground (my/theme-color-get 'text)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-dir-heading nil
                      :weight 'bold
                      :foreground (my/theme-color-get 'green)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-file-name nil
                      :foreground (my/theme-color-get 'text)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-file-suffix nil
                      :foreground (my/theme-color-get 'green)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-number nil
                      :foreground (my/theme-color-get 'yellow)
                      :background (my/theme-color-get 'base))

  (set-face-attribute 'diredfl-date-time nil
                      :foreground (my/theme-color-get 'blue)
                      :background (my/theme-color-get 'base)))

;;(use-package json-navigator
;;  :straight t)

(global-so-long-mode t)

(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode t)
  ;; Use lsp-mode's breadcrumbs for LSP-enabled buffers
  (add-hook 'lsp-mode-hook (lambda () (breadcrumb-local-mode -1))))

(defun find-python-tests-dir ()
  "Find the tests directory based on the dominating pyproject.toml file and additional heuristics."
  (interactive)
  (let ((project-root (locate-dominating-file default-directory "pyproject.toml")))
    (if project-root
        (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
               (potential-dir (concat (file-name-as-directory project-root) project-name))
               (search-dir (if (file-directory-p potential-dir) potential-dir project-root))
               (top-level-tests-dir (concat (file-name-as-directory potential-dir) "tests"))
               (tests-dirs (if (file-directory-p top-level-tests-dir)
                               (list top-level-tests-dir)
                             (directory-files-recursively search-dir "tests$" t))))
          (if tests-dirs
              (progn
                (message "Tests directory found: %s" (car tests-dirs))
                (car tests-dirs))
            (message "Tests directory not found.")))
      (message "pyproject.toml not found in any dominating directory."))))

(defun jump-to-python-tests-dir ()
  "Jump to the tests directory based on the dominating pyproject.toml file and additional heuristics."
  (interactive)
  (let ((tests-dir (find-python-tests-dir)))
    (when tests-dir
      (dired tests-dir))))

(autoload 'pytest-run "pytest")
(autoload 'pytest-get-command "pytest")
(defun pytest-run-current-project ()
  (interactive)
  (let ((tests-dir (find-python-tests-dir)))
    (when-let* (( abs-test-dir (expand-file-name tests-dir))
                (buffer-file-name abs-test-dir))
      (pytest-run (expand-file-name tests-dir) nil))))

(defun pytest-realgud-run-current-project ()
  (interactive)
  (let ((tests-dir (find-python-tests-dir)))
    (when-let* (( abs-test-dir (expand-file-name tests-dir))
                (buffer-file-name abs-test-dir)
                (pytest-command
                 (let ((pytest-cmd-format-string "%2$s %3$s '%4$s'"))
                   (pytest-get-command (expand-file-name tests-dir) nil))))
      (realgud:pdb (format "pytest --pdb %s" tests-dir)))))

(defun pytest-realgud-one (&optional flags)
  (interactive)
  (realgud:pdb (format "pytest --pdb %s %s"  (or flags "") (pytest-py-testable))))

(defun pytest-realgud-module (&optional flags)
  (interactive)
  (realgud:pdb (format "pytest --pdb %s %s"  (or flags "") buffer-file-name)))


(use-package pdb-capf
  :straight t
  :config
  (add-hook 'pdb-track-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pdb-capf nil t))))

(use-package evil-mc
  :straight t
  :config
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

;;(add-to-list 'default-frame-alist
;;             '(font . "Fira Code Medium-12"))

(let ((font-height (if (boundp 'my/font-height) my/font-height 120)))
  (set-face-attribute 'default nil
                      :family "Fira Code" :height font-height :weight 'normal))

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
          (" *Embark Actions*"
           :align below
                            :select t
                            :size 0.3
                            :popup t)
          ("*HTTP Response*"
           :align below
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
          "\\*HTTP Response\\*"
          "\\*Embark Actions\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  :config
  (setq popper-display-control nil))

(use-package ultra-scroll
  :straight (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll" :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 100 
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package consult
   :straight (consult :type git :host github :repo "minad/consult" :branch "main")
   :after projectile
   :defer 0.5
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
          ("<help> a" . describe-symbol))
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

   (defun consult-project-subdir-ripgrep-at-point (arg)
     "Search with `consult-ripgrep` from one directory down from the project root towards the file's directory.
 With a prefix argument ARG, prompt for the search root starting at the project root."
     (interactive "P")
     (let* ((project-root (project-root (project-current t)))
            (file-path (or buffer-file-name
                           (user-error "Buffer is not visiting a file")))
            (relative-path (file-relative-name (directory-file-name (file-name-directory file-path)) project-root))
            (path-components (split-string relative-path "/"))
            (search-root (if arg
                             (read-directory-name "Select directory: " project-root project-root)
                           (if (> (length path-components) 0)
                               (expand-file-name (car path-components) project-root)
                             project-root))))
       (consult-ripgrep search-root (thing-at-point 'symbol))))

   (leader-def :infix "p"
     "a" 'consult-project-ripgrep-at-point)

   (general-define-key
    "C-c s ." 'consult-ripgrep-at-point
    "C-c s p" 'consult-project-ripgrep-at-point
    "C-c s s" 'consult-project-subdir-ripgrep-at-point)


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
               (define-key eshell-mode-map (kbd "M-r") 'consult-history)))
   (run-use-package-config-hooks 'consult))

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
         completion-category-overrides '((file (styles partial-completion))))

   :config
   (setq completion-category-defaults nil)
   (leader-def :infix "f"
     "f" 'find-file)

   (leader-def 
     "x" 'execute-extended-command)

   (leader-def :infix "b"
     "b" 'consult-buffer))


 ;; Optionally add the `consult-flycheck' command.
 (use-package consult-flycheck
   :straight (consult-flycheck :type git :host github :repo "minad/consult" :branch "main")
   :bind (:map flycheck-command-map
               ("!" . consult-flycheck)))

 (use-package embark
   :straight (embark :type git :host github :repo "oantolin/embark")
   :after popper
   :bind
   (:map minibuffer-local-map
         ("C-j" . embark-act))

   :config


   ;; Disabled for now, using shackle/popper for popup
   ;; (add-to-list 'embark-indicators #'embark-which-key-indicator)
   (defun embark-which-key-indicator ()
     "An embark indicator that displays keymaps using which-key.
 The which-key help message will show the type and value of the
 current target followed by an ellipsis if there are further
 targets."
     (lambda (&optional keymap targets prefix)
       (if (null keymap)
           (which-key--hide-popup-ignore-command)
         (which-key--show-keymap
          (if (eq (caar targets) 'embark-become)
              "Become"
            (format "Act on %s '%s'%s"
                    (plist-get (car targets) :type)
                    (embark--truncate-target (plist-get (car targets) :target))
                    (if (cdr targets) "…" "")))
          (if prefix
              (pcase (lookup-key keymap prefix 'accept-default)
                ((and (pred keymapp) km) km)
                (_ (key-binding prefix 'accept-default)))
            keymap)
          nil nil t)))))

(use-package wgrep
  :straight t)


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

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-face-attribute 'fringe nil
                                  :foreground (face-foreground 'default)
                                  :background (face-background 'default)))))

(use-package dimmer
  :straight t
  :config
  (dimmer-mode t)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)

  (defun dimmer-lsp-ui-doc-p ()
    (string-prefix-p " *lsp-ui-doc-" (buffer-name)))
  (add-to-list 'dimmer-prevent-dimming-predicates #'dimmer-lsp-ui-doc-p)

  (defun advices/dimmer-config-change-handler ()
    (dimmer--dbg-buffers 1 "dimmer-config-change-handler")
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (dimmer-process-all (not ignore))))

  (advice-add 'dimmer-config-change-handler :override #'advices/dimmer-config-change-handler)

  (defun corfu-frame-p ()
    "Check if the buffer is a corfu frame buffer."
    (string-match-p "\\` \\*corfu" (buffer-name)))

  (defun dimmer-configure-corfu ()
    "Convenience settings for corfu users."
    (add-to-list
     'dimmer-prevent-dimming-predicates
     #'corfu-frame-p))

  (dimmer-configure-corfu)

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

        (olivetti-mode)
        (flyspell-mode))
    (progn
        (diminish-undo 'olivetti-mode)
        (diminish-undo 'flyspell-mode)

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

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(electric-pair-mode t)

;; Added this because I ran into this issue: https://github.com/copilot-emacs/copilot.el/issues/232
(use-package jsonrpc
  :straight t)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)

  (setq copilot-max-char 30000)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package tdd
  :load-path "site-lisp/tdd/")

(general-define-key
   "C-c t r" 'recompile)

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
  :straight t
  :config
  (dtrt-indent-global-mode)

  (add-to-list 'dtrt-indent-hook-mapping-list '(scss-mode css css-indent-offset))
  (add-to-list 'dtrt-indent-hook-mapping-list '(solidity-mode c/c++/java c-basic-offset)))

(use-package corfu
  :straight t
  :defer 0.1
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 1)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary 'separator)

  (global-corfu-mode))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package git-commit
  :straight (git-commit :type git :host github :repo "magit/magit")
  :after magit)

(use-package magit
  :straight t
  :commands (magit-get-current-branch)
  :defer t
  :config
  ;; Uncomment this to improve performance
  (setq magit-refresh-status-buffer nil)

  ;; If magit-refresh-status-buffer is nil, refresh the magit-status buffer on idle timer 
  (defun magit-refresh-on-idle-timer ()
    (when-let ((buffer (and (not (derived-mode-p 'magit-status-mode))
                            (magit-get-mode-buffer 'magit-status-mode))))
      (when (not magit-refresh-status-buffer)
        (run-with-idle-timer 2 nil (lambda (buffer)
                                     (message "Refreshing magit-status buffer %s" buffer)
                                     (with-current-buffer buffer (magit-refresh-buffer))) buffer))))

  (add-hook 'magit-post-refresh-hook 'magit-refresh-on-idle-timer)

  ;; (setq magit-refresh-verbose t)
  (setf magit-git-environment (append magit-git-environment '("FORCE_COLOR=0"))))

(defun magit-open-pull-request ()
  "Open the pull request on GitHub for the current branch."
  (interactive)
  (require 'magit)
  (browse-url (magit-pull-request-url)))

(defun magit-pull-request-url ()
  "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
  (format "%s/compare/%s"
          (replace-regexp-in-string
           (rx (and string-start (1+ any) "github.com:" (group (1+ any)) ".git" string-end))
           "https://github.com/\\1"
           (magit-get "remote" (magit-get-current-remote) "url"))
          (magit-get-current-branch)))

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

(defun github-commit-url (commit-hash)
  (let* ((base-url (infer-browse-url-from-remote "origin")))
     (format "%s/commit/%s" base-url commit-hash)))

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

(use-package blamer
  :straight t
  :defer 1
  :config
  (setq blamer-force-truncate-long-line t)
  (setq blamer-max-commit-message-length 100)
  (setq blamer-idle-time 1)
  (setq blamer-tooltip-function 'blamer-tooltip-keybindings)

  (set-face-attribute 'blamer-face nil
                      :foreground (my/theme-color-get 'subtext1))

  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p t)
                                            (blamer--clear-overlay)))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p nil)
                                            (copilot-clear-overlay)))

  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash)
      ;; Split window vertically
      (let ((split-height-threshold nil)
            (split-width-threshold 0))
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (browse-url (github-commit-url commit-hash)))))

  (defun blamer-commit-into-at-point ()
    (let* ((line-number (line-number-at-pos))
           (file-name (blamer--get-local-name (buffer-file-name)))
           (blame-cmd-res (when file-name
                            (apply #'vc-git--run-command-string file-name
                                   (append blamer--git-blame-cmd
                                           (list (format "%s,%s" line-number line-number))))))
           (blame-cmd-res (when blame-cmd-res (butlast (split-string blame-cmd-res "\n")))))
      (blamer--parse-line-info (first blame-cmd-res) nil)))

  (defun blamer-open-remote-at-point ()
    (interactive)
    (let ((commit-info (blamer-commit-into-at-point)))
      (blamer-callback-open-remote commit-info)))

  (defun blamer-open-magit-at-point ()
    (interactive)
    (let ((commit-info (blamer-commit-into-at-point)))
      (blamer-callback-show-commit-diff commit-info)))

  (leader-def ".go" 'blamer-open-remote-at-point)
  (leader-def ".gc" 'blamer-open-magit-at-point)

  (setq blamer-type 'visual)

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

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
    clojure-mode-hook
    janet-ts-mode-hook))

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
  :hook ((python-ts-mode . lsp-deferred))
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (make-local-variable 'python-shell-interpreter)
              (make-local-variable 'python-shell-interpreter-args)
              (when (executable-find "ipython")
                (setq python-shell-interpreter "ipython")
                (setq python-shell-interpreter-args "--simple-prompt")))))



(use-package python-black
  :straight t
  :after python
  :hook ((python-mode . python-black-on-save-mode)
         (python-ts-mode . python-black-on-save-mode)))

(use-package python-isort
  :straight t
  :hook (python-ts-mode . python-isort-on-save-mode))

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

(use-package pytest
  :straight t
  :general
  (general-define-key
   :keymaps '(python-mode-map python-ts-mode-map)
   "C-c t ." 'pytest-one
   "C-c t f" 'pytest-module
   "C-c t p" 'pytest-all
   "C-c t r" 'pytest-again)
  :config
  ;; Use pyproject.toml to find a suitable project root for pytest
  ;; This is useful for monorepos
  (add-to-list 'pytest-project-root-files "pyproject.toml")
  ;; Don't use `-x -s` by default, we like capturing output
  (setq pytest-cmd-flags ""))

(use-package realgud
  :straight t

  :config
  (setq realgud:pdb-command-name "pytest --pdb"))

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

(use-package js2-mode
  :straight t
  :hook ((js-mode . js2-minor-mode))
  :config
  (setf js2-mode-show-parse-errors nil)
  (setf js2-strict-missing-semi-warning nil))

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
    (set-face-attribute 'eshell-ls-directory nil
                        :foreground (my/theme-color-get 'sky)
                        :background (my/theme-color-get 'base)))

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
               `(:foreground ,(my/theme-color-get 'red)))

  (esh-section esh-user
               (user-login-name)
               `(:foreground ,(my/theme-color-get 'green)))

  (esh-section esh-dir
               (concat "[" (abbreviate-file-name (eshell/pwd)) "]")
               `(:foreground ,(my/theme-color-get 'mauve)))

  (esh-section esh-git
               (when-let ((branch (magit-get-current-branch))) 
                 (concat " " branch))
               `(:foreground ,(my/theme-color-get 'blue)))

  (esh-section esh-footer
               "\n→"
               `(:foreground ,(my/theme-color-get 'yellow)))

  (setq eshell-prompt-regexp "→ ")
  (setq eshell-skip-prompt-function #'eshell-skip-prompt)
  (setq esh-sep " ")
  (setq esh-funcs (list esh-header esh-user esh-dir esh-git esh-footer))

  (setq eshell-prompt-function 'esh-prompt-func)

  (add-hook 'eshell-mode-hook 'esh-customize-faces))

(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package yaml-pro
  :straight t
  :hook ((yaml-mode . yaml-pro-ts-mode)))

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

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

(use-package go-mode
  :ensure t
  :mode (("go\\.mod\\'" . go-mod-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :hook ((go-ts-mode . lsp-deferred))
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

(use-package bundler
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.rbi$" . ruby-mode))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))


(use-package lsp-mode
  :straight t
  :init
  (setq flymake-allowed-file-name-masks nil)
  :config
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-response-timeout 30)
  (setq lsp-disabled-clients '(pylsp)) 
  (setq lsp-completion-provider :none)

  (with-eval-after-loads '(lsp-mode lsp-pyright lsp-jedi) 

    (defun lsp-disable-all-methods-for-server-except (methods server-id)
      "Disable all methods for SERVER-ID except for METHODS."
      (let* ((server (gethash server-id lsp-clients))
             (all-methods (mapcar 'car lsp-method-requirements)))
        (dolist (method all-methods)
          (unless (seq-contains-p methods method #'string=)
            (lsp-disable-method-for-server method server-id)))))

    ;; Use jedi for finding references since pyright doesn't seem to find all references / hangs a lot
    ;; Disabling because it's too slow / buggy
    ;; (lsp-disable-all-methods-for-server-except '("textDocument/references") 'jedi-sidecar)
    (lsp-disable-method-for-server "textDocument/references" 'pyright))


  ;; Set up emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t))

(use-package lsp-pyright
  :straight t) 

(use-package lsp-jedi
  :straight t
  :config
  ;; Register jedi-language-server so it can run alongside pyright
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () lsp-jedi-executable-command))
    :major-modes '(python-mode python-ts-mode cython-mode)
    :priority -1
    ;; This is the important line
    ;; :add-on? t
    :server-id 'jedi-sidecar
    :library-folders-fn (lambda (_workspace) lsp-jedi-python-library-directories)
    :initialization-options (lambda () (gethash "jedi" (lsp-configuration-section "jedi"))))))

(use-package rust-mode
  :ensure t
  :after general
  :hook ((rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred))
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t))


(with-eval-after-loads '(rust-mode lsp-mode)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-inlay-hint-enable t)
  (setq lsp-eldoc-render-all nil)

  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-inlay-hints-mode t))

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

(use-package prettier
  :straight t
  :config)

(use-package vyper-mode
  :straight t)

(use-package verb
  :straight t
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((verb . t))))
  (setq verb-auto-kill-response-buffers t)
  )

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'" . terraform-mode))

(setq treesit-language-source-alist
      (if (eq 'windows-nt system-type)
          '((janet-simple
             . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                nil nil "gcc.exe")))
        '((janet-simple
           . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

(when (not (treesit-language-available-p 'janet-simple))
  (treesit-install-language-grammar 'janet-simple))

(use-package janet-ts-mode
  :straight (janet-ts-mode :type git :host github :repo "sogaiu/janet-ts-mode")
  :mode (("\\.janet\\'" . janet-ts-mode)))

(use-package ajrepl
  :straight (ajrepl :host github
                    :repo "sogaiu/ajrepl"
                    :files ("*.el" "ajrepl"))
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

(with-eval-after-loads '(janet-ts-mode lsp-mode) 
  (add-to-list 'lsp-language-id-configuration
               '(janet-ts-mode . "janet"))

  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "janet-lsp")
                        :activation-fn (lsp-activate-on "janet")
                        :server-id 'janet-lsp)))

(with-eval-after-loads '(janet-ts-mode)
  (require 'reformatter)

  (defgroup janet-format nil
    "Python reformatting using black."
    :group 'janet
    :prefix "janet-format-")

  (reformatter-define janet-format
    :program "janet-format"
    :group 'janet-format)

  (add-hook 'janet-ts-mode-hook 'janet-format-on-save-mode))

(use-package kubed
  :straight (kubed :type git :host github :repo "eshelyaron/kubed"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

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

(use-package org-contrib
  :straight t)

(use-package org
  :straight t
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
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:LOGBOOK:\n- Added %U\n:END:\n%a\n")))

  (run-use-package-config-hooks 'org-capture))

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

(use-package lru
  :straight (lru :type git :host github :repo "landakram/lru"))

(use-package ready-player
  :ensure t)

(use-package gptel
  :straight t
  :bind
  ("C-c RET" . gptel-send)
  :config

  (setq gptel-default-mode 'org-mode)

  (setq gptel-expert-commands t)
  )


(use-package elysium
  :straight (elysium :type git :host github :repo "lanceberge/elysium"))

(use-package smerge-mode
  :straight t
  :hook
  (prog-mode . smerge-mode))

(use-package aidermacs
  :straight (aidermacs :type git :host github :repo "MatthewZMD/aidermacs" :branch "main")
  :bind (("C-c p" . aidermacs-transient-menu))

  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)

  :custom
  ; See the Configuration section below
  (aidermacs-auto-commits t)
  (aidermacs-default-model "o3-mini"))
