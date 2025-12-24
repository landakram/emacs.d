;;; editor/navigation.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

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

(provide 'editor/navigation)
;;; editor/navigation.el ends here
