;;; lang/javascript.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package js2-mode
  :straight t
  :hook ((js-mode . js2-minor-mode))
  :config
  (setf js2-mode-show-parse-errors nil)
  (setf js2-strict-missing-semi-warning nil))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode . add-node-modules-path)
         (solidity-mode . add-node-modules-path)))

(use-package nvm
  :straight t
  :hook ((js-mode . nvm-use-for)
         (solidity-mode . nvm-use-for)))

 (defun js2-imenu-make-index ()
    (save-excursion
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)

                                ;;add more keyword for mocha here
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)

                               ))))

 (add-hook 'js2-minor-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'js2-imenu-make-index)))


(use-package js-comint
  :defer t
  :ensure t)

(setq js-indent-level 2)

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :ensure t)

(provide 'lang/javascript)
;;; lang/javascript.el ends here
