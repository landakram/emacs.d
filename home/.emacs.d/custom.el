(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(elfeed-feeds (quote ("http://lambda-the-ultimate.org/rss.xml")))
 '(fci-rule-color "#515151")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (perfect-margin bundler request-deferred request string-inflection etags-select shackle outshine smart-tab prodigy window-purpose evil-mc cljsbuild-mode emmet-mode restclient org-bullets auctex smex ledger-mode expand-region php-mode pug-mode zenburn-theme yaml-mode which-key web-mode web visual-fill-column use-package try swift-mode smart-mode-line sicp pythonic py-yapf projectile popup pinentry paradox ox-rst ox-jira ox-gfm org-plus-contrib org-jira nvm monokai-theme mmm-jinja2 markdown-mode magithub lua-mode load-theme-buffer-local js-comint ivy-hydra ignoramus haskell-mode gh general geiser flycheck flx exec-path-from-shell evil-surround evil-mu4e evil-matchit evil-magit evil-cleverparens ember-mode elpy elpakit elfeed dtrt-indent dockerfile-mode docker counsel color-theme-zenburn color-theme-sanityinc-tomorrow color-theme-buffer-local coffee-mode clj-refactor circe better-defaults ag ace-window)))
 '(paradox-github-token t)
 '(pdf-info-epdfinfo-program
   "/Users/mark/.emacs.d/elpa/pdf-tools-20160203.1057/build/server/epdfinfo")
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((cider-default-cljs-repl . "figwheel-main")
     (cider-preferred-build-tool . "lein")
     (eval and
           (set
            (make-local-variable
             (quote my-project-path))
            (file-name-directory
             (let
                 ((d
                   (dir-locals-find-file ".")))
               (if
                   (stringp d)
                   d
                 (car d)))))
           (when
               (not
                (eq tags-table-list
                    (list
                     (concat my-project-path "TAGS"))))
             (setq tags-table-list
                   (list
                    (concat my-project-path "TAGS")))))
     (eval and
           (set
            (make-local-variable
             (quote my-project-path))
            (file-name-directory
             (let
                 ((d
                   (dir-locals-find-file ".")))
               (if
                   (stringp d)
                   d
                 (car d)))))
           (when
               (not
                (eq tags-table-list
                    (list
                     (concat my-project-path "TAGS"))))
             (setq tags-table-list
                   (list
                    (concat my-project-path "TAGS")))
             (visit-tags-table
              (car tags-table-list))))
     (eval and
           (set
            (make-local-variable
             (quote my-project-path))
            (file-name-directory
             (let
                 ((d
                   (dir-locals-find-file ".")))
               (if
                   (stringp d)
                   d
                 (car d)))))
           (setq tags-table-list
                 (list
                  (concat my-project-path "TAGS")))
           (visit-tags-table
            (car tags-table-list)))
     (eval and
           (set
            (make-local-variable
             (quote my-project-path))
            (file-name-directory
             (let
                 ((d
                   (dir-locals-find-file ".")))
               (if
                   (stringp d)
                   d
                 (car d)))))
           (setq tags-table-list
                 (list
                  (concat my-project-path "TAGS")))
           (visit-tags-table tags-table
                             (car tags-table-list)))
     (eval and
           (set
            (make-local-variable
             (quote my-project-path))
            (file-name-directory
             (let
                 ((d
                   (dir-locals-find-file ".")))
               (if
                   (stringp d)
                   d
                 (car d)))))
           (setq tags-table-list
                 (list
                  (concat my-project-path "TAGS"))))
     (eval and
           (setq dir-local-dir
                 (locate-dominating-file
                  (if
                      (string-empty-p
                       (buffer-file-name))
                      (projectile-project-root)
                    (buffer-file-name))
                  ".dir-locals.el"))
           (setq tags-table-list
                 (list
                  (concat dir-local-dir "TAGS"))))
     (eval and
           (setq dir-local-dir
                 (locate-dominating-file
                  (buffer-file-name)
                  ".dir-locals.el"))
           (setq tags-table-list
                 (list
                  (concat dir-local-dir "TAGS"))))
     (tags-table-list quote
                      ("~/Projects/php-repos/TAGS"))
     (setq tags-table-list
           (quote
            ("~/Projects/php-repos/TAGS")))
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

