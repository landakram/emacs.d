;;; browser.el --- Fast checkout browsing -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar my/browser-roots nil
  "Canonical local checkout directories, with trailing slashes.
When nil, enable the browser in any local Git checkout.")
(defvar my/browser-search-roots-function #'list
  "Function returning definition and reference directories for a checkout root.")
(defvar my/browser-git-command-function nil
  "Optional function returning a Git command list for a checkout root.")
(defvar my/browser-script (expand-file-name "bin/emacs-browser.py" user-emacs-directory))
(defvar my/browser-python (if (file-executable-p "/usr/bin/python3") "/usr/bin/python3" "python3"))
(defvar my/browser-cache-directory
  (expand-file-name "browser-cache/" user-emacs-directory))
(defvar my/browser-cache-stamps (make-hash-table :test 'equal))

(defvar-local my/browser--last-directory nil)
(defvar my/browser-families (make-hash-table :test 'equal))

(defun my/browser-read-line (file)
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (string-trim-right (buffer-string))))

(defun my/browser-common-directory (root)
  (let ((marker (expand-file-name ".git" root)))
    (when (file-regular-p marker)
      (let ((pointer (my/browser-read-line marker)))
        (when (string-prefix-p "gitdir: " pointer)
          (let* ((admin (expand-file-name (substring pointer 8) root))
                 (common (expand-file-name "commondir" admin)))
            (when (file-readable-p common)
              (file-truename (expand-file-name (my/browser-read-line common) admin)))))))))

(defun my/browser-family-at-root (root)
  "Return the configured canonical checkout for ROOT, including linked worktrees."
  (if (or (null my/browser-roots) (member root my/browser-roots)) root
    (when-let* ((common (my/browser-common-directory root)))
      (cl-find-if (lambda (dir) (equal common (expand-file-name ".git" dir)))
                  my/browser-roots))))

(defun my/browser-root (&optional path)
  "Return this file's local browsing checkout without invoking Git."
  (let ((path (or (and path (not (file-remote-p path)) (file-directory-p path)
                       (file-name-as-directory path))
                  path buffer-file-name default-directory)))
    (unless (file-remote-p path)
      (let ((dir (file-name-directory (expand-file-name path))))
        (unless (equal dir (car my/browser--last-directory))
          (let* ((top (locate-dominating-file dir ".git"))
                 (root (and top (file-name-as-directory (file-truename top))))
                 (family (and root (my/browser-family-at-root root))))
            (when family (puthash root family my/browser-families))
            (setq my/browser--last-directory (cons dir (and family root)))))
        (cdr my/browser--last-directory)))))

(defun my/browser-read-root ()
  "Offer existing registered worktrees as well as the canonical checkouts."
  (let ((roots (copy-sequence my/browser-roots)))
    (dolist (canonical my/browser-roots)
      (let ((admin (expand-file-name ".git/worktrees" canonical)))
        (when (file-directory-p admin)
          (dolist (entry (directory-files admin t directory-files-no-dot-files-regexp))
            (let ((pointer (expand-file-name "gitdir" entry)))
              (when (file-readable-p pointer)
                (let ((marker (expand-file-name (my/browser-read-line pointer) entry)))
                  (when (file-regular-p marker)
                    (cl-pushnew (file-name-directory marker) roots :test #'equal)))))))))
    (if roots
        (completing-read "Browse checkout: " roots nil t)
      (or (my/browser-root (read-directory-name "Browse Git checkout: " nil nil t))
          (user-error "Choose a directory in a Git checkout")))))

(defun my/browser-cache-name (root)
  (concat "tree-" (secure-hash 'sha256 (encode-coding-string (directory-file-name root) 'utf-8))))

(defvar my/browser-use-times (make-hash-table :test 'equal))

(defun my/browser-note-use (root)
  "Mark a browsed worktree for morning indexing, at most once per hour."
  (when (and root (> (- (float-time) (gethash root my/browser-use-times 0)) 3600)
             (file-regular-p (expand-file-name ".git" root)))
    (condition-case err
        (progn
          (make-directory my/browser-cache-directory t)
          (write-region "" nil (expand-file-name (concat (my/browser-cache-name root) ".used")
                                                  my/browser-cache-directory) nil 'silent)
          (puthash root (float-time) my/browser-use-times))
      (file-error (message "Could not record checkout use: %s" (error-message-string err))))))

(defun my/browser-note-file-visit ()
  (when buffer-file-name (my/browser-note-use (my/browser-root))))

(add-hook 'find-file-hook #'my/browser-note-file-visit)

(defun my/browser-command (action root)
  (list my/browser-python my/browser-script "--cache-dir" my/browser-cache-directory action root))

(defun my/browser-start-index (root action)
  (let ((name (format "browser-%s-%s" action (my/browser-cache-name root))))
    (unless (get-process name)
      (make-process :name name :buffer (concat "*" name "*") :noquery t
                    :command (my/browser-command action root)
                    :sentinel (lambda (process event)
                                (when (or (equal action "index")
                                          (not (zerop (process-exit-status process))))
                                  (message "%s: %s" (process-name process) (string-trim event))))))))

(defun my/browser-find-file (&optional choose)
  "Fuzzy-find files belonging to this checkout. With CHOOSE, pick a checkout."
  (interactive "P")
  (require 'consult)
  (let* ((root (or (and (not choose) (my/browser-root)) (my/browser-read-root)))
         (default-directory root)
         (this-command 'my/browser-find-file)
         (consult-locate-args (my/browser-command "find" root))
         (consult-async-min-input 2)
         (consult-preview-key "M-."))
    ;; Index independently of the minibuffer workers that Consult cancels as you type.
    (my/browser-note-use root)
    (my/browser-start-index root "ensure")
    (consult-locate)))

(with-eval-after-load 'consult
  (consult-customize my/browser-find-file :state (consult--file-preview)))

(defun my/browser-projectile-find (original &rest args)
  "Route the existing Projectile shortcut through the asynchronous browser."
  (if (my/browser-root)
      (my/browser-find-file)
    (apply original args)))

(defun my/browser-refresh-index ()
  "Refresh the current checkout's files, including newly created paths."
  (interactive)
  (my/browser-start-index (or (my/browser-root) (my/browser-read-root)) "index"))

(defun my/browser-projectile-files (original root)
  "Read the saved native index if another Projectile command needs all files.
The normal file picker avoids loading this million-entry list into Emacs."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (index (expand-file-name (concat (my/browser-cache-name root) ".files0")
                                  my/browser-cache-directory)))
    (if (not (equal root (my/browser-root root)))
        (funcall original root)
      (unless (file-readable-p index)
        (unless (zerop (apply #'call-process my/browser-python nil nil nil (cdr (my/browser-command "ensure" root))))
          (user-error "Could not index files in %s" root)))
      (let ((stamp (file-attribute-modification-time (file-attributes index))))
        (unless (and (equal stamp (gethash root my/browser-cache-stamps))
                     (gethash root projectile-projects-cache))
          (puthash root
                   (with-temp-buffer
                     (insert-file-contents index)
                     (split-string (buffer-string) "\0" t))
                   projectile-projects-cache)
          (puthash root stamp my/browser-cache-stamps))
        (gethash root projectile-projects-cache)))))

(with-eval-after-load 'projectile
  (advice-add 'projectile-find-file :around #'my/browser-projectile-find)
  (advice-add 'projectile-project-files :around #'my/browser-projectile-files))

(defun my/browser-skip-automatic (original &rest args)
  "Skip automatic development services in browsing checkouts."
  (unless (and (my/browser-root)
               (not (called-interactively-p 'any))
               (not (equal (car args) -1)))
    (apply original args)))

(dolist (function '(lsp-deferred eglot-ensure))
  (advice-add function :around #'my/browser-skip-automatic))

(defun my/browser-open (original filename &rest args)
  "Avoid per-file Git status and executable directory-local setup while browsing."
  (if (not (my/browser-root filename))
      (apply original filename args)
    (let ((vc-handled-backends nil)
          (enable-local-eval nil))
      (apply original filename args))))
(advice-add 'find-file-noselect :around #'my/browser-open)

;; Missing or ABI-incompatible grammars must never prevent opening a file.
(defvar my/browser-language-modes
  '((python python-ts-mode python-mode "\\.py\\'")
    (rust rust-ts-mode rust-mode "\\.rs\\'")
    (typescript typescript-ts-mode typescript-mode "\\.ts\\'")
    (tsx tsx-ts-mode typescript-mode "\\.tsx\\'")
    (javascript js-ts-mode js-mode "\\.jsx?\\'")
    (json json-ts-mode json-mode "\\.json\\'")
    (yaml yaml-ts-mode yaml-mode "\\.ya?ml\\'")
    (css css-ts-mode css-mode "\\.css\\'")))

(defun my/browser-configure-highlighting ()
  "Use available tree-sitter grammars and installed classic modes as fallback."
  (dolist (entry my/browser-language-modes)
    (pcase-let ((`(,language ,ts-mode ,fallback ,pattern) entry))
      (let ((ready (and (fboundp 'treesit-language-available-p)
                        (treesit-language-available-p language))))
        (setq major-mode-remap-alist
              (assq-delete-all fallback major-mode-remap-alist))
        (add-to-list 'auto-mode-alist (cons pattern (if ready ts-mode fallback))))))
  (unless (treesit-language-available-p 'bash)
    (setq major-mode-remap-alist (assq-delete-all 'bash-mode major-mode-remap-alist)))
  (global-font-lock-mode 1))

(my/browser-configure-highlighting)
(global-set-key (kbd "C-c f") #'my/browser-find-file)
(global-set-key (kbd "C-c r") #'my/browser-refresh-index)

(with-eval-after-load 'blamer (require 'editor/browser-blame))
(require 'editor/browser-navigation)

(provide 'editor/browser)
;;; browser.el ends here
