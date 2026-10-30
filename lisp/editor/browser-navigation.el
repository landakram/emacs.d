;;; browser-navigation.el --- Source jumps without servers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'xref)
(require 'dumb-jump)

(defvar consult-preview-key)
(defvar consult-ripgrep-args)
(defvar consult-async-min-input)
(defvar my/browser-definition-roots nil)

(defconst my/browser-reference-ripgrep-args
  '("rg" "--null" "--line-buffered" "--color=never"
    "--max-columns=1000" "--path-separator" "/" "--case-sensitive"
    "--no-heading" "--with-filename" "--line-number" "--word-regexp"
    "--fixed-strings" "--glob" "!node_modules" "--glob" "!target"
    "--glob" "!.venv" "--glob" "!bazel-*"))

(defun my/browser-definition-search-roots ()
  "Choose the search directories for the current checkout."
  (funcall my/browser-search-roots-function (my/browser-root)))

(defun my/browser-dumb-jump-backend () 'browser-dumb-jump)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql browser-dumb-jump)))
  (xref-backend-identifier-at-point 'dumb-jump))

(cl-defmethod xref-backend-definitions ((_backend (eql browser-dumb-jump)) identifier)
  (let ((dumb-jump-force-searcher 'rg)
        (dumb-jump-rg-cmd "rg")
        (dumb-jump-rg-search-args
         "--pcre2 --glob '!node_modules' --glob '!target' --glob '!.venv' --glob '!bazel-*'")
        (dumb-jump-aggressive nil)
        (dumb-jump-ignore-context t)
        (dumb-jump-fallback-search nil)
        (dumb-jump-quiet t))
    (delete-dups
     (cl-mapcan (lambda (root)
                  (let ((dumb-jump-project root))
                    (xref-backend-definitions 'dumb-jump identifier)))
                my/browser-definition-roots))))

(defun my/browser-record-jump ()
  (when (bound-and-true-p jumpy-mode)
    (jumpy--log-if-necessary (slot-value jumpy-mode 'backtracker) t)))

(defun my/browser-goto-definition (original &rest args)
  "Use Dumb Jump for browsing; with a prefix, choose the search directory."
  (if (not (my/browser-root))
      (apply original args)
    (require 'consult)
    (let* ((roots (my/browser-definition-search-roots))
           (my/browser-definition-roots
            (if current-prefix-arg
                (list (read-directory-name "Search definitions under: " (car roots) nil t))
              roots))
           (xref-backend-functions '(my/browser-dumb-jump-backend))
           (xref-show-definitions-function #'consult-xref)
           (consult-preview-key "M-.")
           (identifier (xref-backend-identifier-at-point 'dumb-jump)))
      (unless identifier (user-error "No symbol under cursor"))
      (my/browser-record-jump)
      (xref-find-definitions identifier)
      (my/browser-record-jump))))

(defun my/browser-find-references (&optional choose)
  "Find text occurrences of the symbol asynchronously, without LSP.
With CHOOSE, select the search directory.  Includes definitions and comments."
  (interactive "P")
  (require 'consult)
  (let* ((identifier (or (thing-at-point 'symbol t)
                         (user-error "No symbol under cursor")))
         (roots (if (my/browser-root) (my/browser-definition-search-roots)
                  (list default-directory)))
         (roots (if choose
                    (list (read-directory-name "Search references under: " (car roots) nil t))
                  roots))
         (consult-ripgrep-args my/browser-reference-ripgrep-args)
         (consult-async-min-input 1)
         (consult-preview-key "M-."))
    (my/browser-record-jump)
    (consult-ripgrep roots identifier)
    (my/browser-record-jump)))

(with-eval-after-load 'evil
  (advice-add 'evil-goto-definition :around #'my/browser-goto-definition)
  (define-key evil-normal-state-map (kbd "g r") #'my/browser-find-references))

(provide 'editor/browser-navigation)
;;; browser-navigation.el ends here
