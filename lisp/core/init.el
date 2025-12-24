;;; core/init.el --- bootstrap dispatcher -*- lexical-binding: t; -*-

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core/utils)
(require 'core/startup)
(require 'core/packages)
(require 'core/perf)

(require 'util/base)
(require 'os/env)

(require 'editor/core)
(require 'editor/keymaps)
(require 'ui/core)
(require 'editor/navigation)
(require 'editor/writing)

(require 'completion/core)

(require 'tools/git)

(require 'lang/base)
(dolist (module '(lang/lisp
                  lang/python
                  lang/javascript
                  lang/docker
                  lang/shell
                  lang/go
                  lang/ruby
                  lang/rust
                  lang/swift
                  lang/json
                  lang/markdown
                  lang/haskell
                  lang/yaml
                  lang/lua
                  lang/html
                  lang/latex
                  lang/vim
                  lang/extras))
  (require module))

(require 'org/core)

(dolist (app '(apps/email
               apps/rss
               apps/irc
               apps/budgeting
               apps/gopher
               apps/media
               apps/gpt))
  (require app))

(provide 'core/init)
;;; core/init.el ends here
