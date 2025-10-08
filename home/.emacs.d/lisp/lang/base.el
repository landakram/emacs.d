;;; lang/base.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

;; Added this because I ran into this issue: https://github.com/copilot-emacs/copilot.el/issues/232
(use-package jsonrpc
  :straight t)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)

  (setq copilot-max-char 30000)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package tdd
  :load-path "site-lisp/tdd/")

(general-define-key
   "C-c t r" 'recompile)

(defun local/postprocess-compilation-buffer ()
  (goto-char compilation-filter-start)
  (when (looking-at "\033c")
    (delete-region (point-min) (match-end 0)))
  (ansi-color-apply-on-region (point) (point-max)))

(add-hook 'compilation-filter-hook 'local/postprocess-compilation-buffer)

(setq compilation-scroll-output 'first-error)

(defun compilation-mode-common-search-paths (orig-fn &rest args)
  (let* ((project-root (car (project-roots (project-current))))
         (compilation-search-path
          (list
           project-root
           (concat (file-name-as-directory project-root) "node_modules"))))
    (prin1 compilation-search-path)
    (apply orig-fn args)))

(advice-add 'compilation-find-file :around #'compilation-mode-common-search-paths)

(add-to-list 'compilation-error-regexp-alist 'mocha)
(add-to-list 'compilation-error-regexp-alist 'mocha-abs)

(add-to-list 'compilation-error-regexp-alist-alist
             '(mocha "at.*(\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\))" 1 2 3))

(add-to-list 'compilation-error-regexp-alist-alist
             '(mocha-abs "at \\([^ ]+?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(use-package dtrt-indent
  :straight t
  :config
  (dtrt-indent-global-mode)

  (add-to-list 'dtrt-indent-hook-mapping-list '(scss-mode css css-indent-offset))
  (add-to-list 'dtrt-indent-hook-mapping-list '(solidity-mode c/c++/java c-basic-offset)))

(use-package corfu
  :straight t
  :defer 0.1
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 1)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary 'separator)

  (global-corfu-mode))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package yasnippet
  :ensure t
  :defer 0.1
  :config

  ;; Make Yasnippet work in Org
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))


(use-package lsp-mode
  :straight t
  :init
  (setq flymake-allowed-file-name-masks nil)
  :config
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-response-timeout 30)
  (setq lsp-disabled-clients '(pylsp)) 
  (setq lsp-completion-provider :none)

  (with-eval-after-loads '(lsp-mode lsp-pyright lsp-jedi) 

    (defun lsp-disable-all-methods-for-server-except (methods server-id)
      "Disable all methods for SERVER-ID except for METHODS."
      (let* ((server (gethash server-id lsp-clients))
             (all-methods (mapcar 'car lsp-method-requirements)))
        (dolist (method all-methods)
          (unless (seq-contains-p methods method #'string=)
            (lsp-disable-method-for-server method server-id)))))

    ;; Use jedi for finding references since pyright doesn't seem to find all references / hangs a lot
    ;; Disabling because it's too slow / buggy
    ;; (lsp-disable-all-methods-for-server-except '("textDocument/references") 'jedi-sidecar)
    (lsp-disable-method-for-server "textDocument/references" 'pyright))


  ;; Set up emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t))

(use-package lsp-pyright
  :straight t) 

(use-package lsp-jedi
  :straight t
  :config
  ;; Register jedi-language-server so it can run alongside pyright
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () lsp-jedi-executable-command))
    :major-modes '(python-mode python-ts-mode cython-mode)
    :priority -1
    ;; This is the important line
    ;; :add-on? t
    :server-id 'jedi-sidecar
    :library-folders-fn (lambda (_workspace) lsp-jedi-python-library-directories)
    :initialization-options (lambda () (gethash "jedi" (lsp-configuration-section "jedi"))))))

(provide 'lang/base)
;;; lang/base.el ends here
