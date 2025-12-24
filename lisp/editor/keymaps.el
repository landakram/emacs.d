;;; editor/keymaps.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

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
                     :def (lambda () (interactive) (find-file "~/.emacs.d/lisp/")))
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

  (setq evil-want-minibuffer nil)

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

(provide 'editor/keymaps)
;;; editor/keymaps.el ends here
