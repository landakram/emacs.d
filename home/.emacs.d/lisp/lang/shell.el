;;; lang/shell.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(use-package eshell
  :commands (eshell)
  :config
  (defun esh-customize-faces ()
    (set-face-attribute 'eshell-ls-directory nil
                        :foreground (my/theme-color-get 'sky)
                        :background (my/theme-color-get 'base)))

  (defmacro esh-section (name form &rest props)
    `(setq ,name
           (lambda ()
             (when ,form
               (-> ,form
                   (propertize 'face (list ,@props)))))))

  (defun esh-acc (acc x)
    (if-let ((section (funcall x)))
        (if (string-empty-p acc)
            section
          (concat acc esh-sep section))
      acc))

  (defun esh-prompt-func ()
    (concat
     (reduce #'esh-acc esh-funcs :initial-value "")
     ;; Reset face to default for input
     (propertize " " 'face 'default)))

  (esh-section esh-header
               "λ"
               `(:foreground ,(my/theme-color-get 'red)))

  (esh-section esh-user
               (user-login-name)
               `(:foreground ,(my/theme-color-get 'green)))

  (esh-section esh-dir
               (concat "[" (abbreviate-file-name (eshell/pwd)) "]")
               `(:foreground ,(my/theme-color-get 'mauve)))

  (esh-section esh-git
               (when-let ((branch (magit-get-current-branch))) 
                 (concat " " branch))
               `(:foreground ,(my/theme-color-get 'blue)))

  (esh-section esh-footer
               "\n→"
               `(:foreground ,(my/theme-color-get 'yellow)))

  (setq eshell-prompt-regexp "→ ")
  (setq eshell-skip-prompt-function #'eshell-skip-prompt)
  (setq esh-sep " ")
  (setq esh-funcs (list esh-header esh-user esh-dir esh-git esh-footer))

  (setq eshell-prompt-function 'esh-prompt-func)

  (add-hook 'eshell-mode-hook 'esh-customize-faces))

(provide 'lang/shell)
;;; lang/shell.el ends here
