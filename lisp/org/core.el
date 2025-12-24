;;; org/core.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


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

(provide 'org/core)
;;; org/core.el ends here
