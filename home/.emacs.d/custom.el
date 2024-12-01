(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream)
           (magit-disable-section-inserter
            'magit-insert-unpushed-to-upstream-or-recent)
           (magit-disable-section-inserter 'magit-insert-stashes)
           (magit-disable-section-inserter 'magit-insert-unpushed-to-pushremote)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-pushremote))
     (eval progn (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream)
           (magit-disable-section-inserter
            'magit-insert-unpushed-to-upstream-or-recent)
           (magit-disable-section-inserter 'magit-insert-stashes)
           (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpushed-to-pushremote)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-pushremote))
     (eval progn (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream)
           (magit-disable-section-inserter
            'magit-insert-unpushed-to-upstream-or-recent)
           (magit-disable-section-inserter 'magit-insert-stashes)
           (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpushed-to-pushremote))
     (eval progn (magit-disable-section-inserter 'magit-insert-tags-header)
           (magit-disable-section-inserter 'magit-insert-unpulled-from-upstream)
           (magit-disable-section-inserter
            'magit-insert-unpushed-to-upstream-or-recent)
           (magit-disable-section-inserter 'magit-insert-stashes)
           (magit-disable-section-inserter 'magit-insert-tags-header))
     (forge-add-pullreq-refspec)
     (eval setq python-black-extra-args
           (list "--config"
                 (expand-file-name "pyproject.toml" (project-root (project-current)))))
     (eval add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil t)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

