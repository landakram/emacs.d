;;; apps/irc.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:

(setq my-credentials-file "~/.private.el")

(defun my/nickserv-password (_)
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :nickserv-password)))

(use-package circe
  :ensure t
  :defer t
  :config

  (setq circe-network-options
        `(("Freenode"
           :nick "landakram"
           :channels (:after-auth
                      "#emacs"
                      "#clojure"
                      "#clojure-beginners"
                      "#iphonedev"
                      "#swift-lang"
                      "#racket"
                      "#chicken"
                      "#lisp"
                      "#stumpwm"
                      "#archlinux"
                      "#ethereum"
                      "#ethereum-dev"
                      "#bitcoin"
                      "#bitcoin-core-dev"
                      "#ipfs"
                      "#n-o-d-e"
                      "#ruby")
           :nickserv-password ,(my/nickserv-password nil)
           :reduce-lurker-spam t)))
  (enable-circe-color-nicks))

(provide 'apps/irc)
;;; apps/irc.el ends here
