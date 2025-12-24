;;; core/utils.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(defun eval-after-load-all (my-features form)
  "Run FORM after all MY-FEATURES are loaded.
See `eval-after-load' for the possible formats of FORM."
  (if (null my-features)
      (if (functionp form)
          (funcall form)
        (eval form))
    (eval-after-load (car my-features)
      `(lambda ()
         (eval-after-load-all
          (quote ,(cdr my-features))
          (quote ,form))))))

(defmacro with-eval-after-loads (my-features &rest body)
  (declare (indent 1) (debug (form def-body)))
  `(eval-after-load-all ,my-features (lambda () ,@body)))

(defun image-to-base64-data-url (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     (concat "data:" mime-type ";base64," base64-string)))

(defun image-to-base64 (image-path)
  "Converts an image at IMAGE-PATH to a Base64-encoded data URL."
  (interactive "fSelect image file: ")
  (let* ((image-type (file-name-extension image-path))
         (mime-type (cond ((string= image-type "png") "image/png")
                          ((string= image-type "jpg") "image/jpeg")
                          ((string= image-type "jpeg") "image/jpeg")
                          ((string= image-type "gif") "image/gif")
                          (t (error "Unsupported image type"))))
         (base64-string (with-temp-buffer
                          (insert-file-contents-literally image-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string))))
     base64-string))

(provide 'core/utils)
;;; core/utils.el ends here
