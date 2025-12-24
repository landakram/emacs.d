;;; completion/core.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(ido-mode -1)

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


;; (use-package visual-fill-column
;;   :ensure t
;;   :diminish visual-line-mode
;;   :config
;;   (add-hook 'text-mode-hook 'visual-fill-column-mode)
;;   (global-visual-line-mode))

(provide 'completion/core)
;;; completion/core.el ends here
