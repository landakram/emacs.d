;;; util/base.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(defun shell-command-ignore-stderr (some-command)
  (with-output-to-string
    (with-current-buffer standard-output
      (process-file shell-file-name nil '(t nil)  nil shell-command-switch some-command))))

(use-package request
  :ensure t)

(use-package request-deferred
  :ensure t)

(use-package deferred
  :ensure t)

(use-package concurrent
  :ensure t)

(use-package s
  :ensure t)

(defun snake-case-thing-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (thing (buffer-substring-no-properties start end))
         (new-thing (s-snake-case
                     (s-lower-camel-case thing))))
    (delete-region start end)
    (insert new-thing)))

(defun camel-case-thing-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (thing (buffer-substring-no-properties start end))
         (new-thing (s-lower-camel-case thing)))
    (delete-region start end)
    (insert new-thing)))

(use-package dash
  :ensure t)

(use-package ht
  :ensure t)

(use-package parse-csv
  :straight (parse-csv :type git :host github :repo "mrc/el-csv")
  :config
  (defun csv-row-get (header row key)
    (let ((index (-find-index (-partial #'equal key) header)))
      (nth index row)))

  (defun venmo-to-ynab-row (header row)
    (let* ((memo (->> (or (csv-row-get header row "Note") "")
                      (s-collapse-whitespace)))
           (date (ignore-errors
                   (--> (csv-row-get header row "Datetime")
                        (parse-iso8601-time-string it))))
           (ynab-date (and (car date) (format-time-string "%m/%d/%Y" date)))
           (amount (-some->> (csv-row-get header row "Amount (total)")
                     (s-replace " $" "")
                     (s-replace "," "")
                     (string-to-number)))
           (inflow (when (and amount (>= amount 0))
                     (format "%0.2f" (abs amount))))
           (outflow (when (and amount (< amount 0))
                      (format "%0.2f" (abs amount))))
           (payee (if inflow
                      (csv-row-get header row "From")
                    (csv-row-get header row "To"))))
      (when (and (or inflow outflow)
                 ynab-date)
        (list ynab-date payee memo outflow inflow))))

  (defun venmo-to-ynab ()
    (interactive)
    (let* ((venmo-statement-filename (read-file-name
                                      "Venmo statement: "
                                      "~/Downloads/venmo_statement.csv"))
           (venmo-statement (--> venmo-statement-filename
                                 (with-temp-buffer
                                   (insert-file-contents it)
                                   (buffer-string))
                                 (parse-csv-string-rows it  ?\, ?\" "\n")))
           (header (car venmo-statement))
           (body (cdr venmo-statement))
           (new-body 
            (->> body
                 (mapcar (-partial #'venmo-to-ynab-row header))
                 (remove nil)))
           (new-header '("Date" "Payee" "Memo" "Outflow" "Inflow"))
           (ynab-statement-filename "~/Downloads/venmo_ynab_statement.csv"))
      (with-temp-file ynab-statement-filename
        (insert (s-join "," new-header) "\n")
        (dolist (row new-body)
          (insert (s-join "," row) "\n")))

      (let ((default-directory "~/Downloads"))
        (shell-command "open -R .")))))

(provide 'util/base)
;;; util/base.el ends here
