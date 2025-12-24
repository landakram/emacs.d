;;; ui/core.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

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

(provide 'ui/core)
;;; ui/core.el ends here
