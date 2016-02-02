(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(defvar my-packages '(better-defaults
                      evil
                      helm
                      paredit
                      projectile
                      helm-projectile
                      magit
                      evil-magit
                      evil-leader))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)


;;;; Helm

(require 'helm)
(require 'helm-config)
(require 'helm-locate)
(require 'helm-projectile)

(helm-projectile-on)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Use helm for finding files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Use helm for M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Use helm to show the kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Use helm for buffer list
(global-set-key (kbd "C-x b") 'helm-mini)


(helm-mode 1)


;;;;;;

;; Keyboard shortcuts for increasing and decreasing text size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;;; Evil Mode 
(require 'evil)
(require 'evil-leader)
(require 'evil-magit)

;; Move between buffers with hjkl

(evil-leader/set-leader "<SPC>")

(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "f" 'helm-find-files)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'kill-buffer-and-window)
(evil-leader/set-key "b" 'helm-mini)
(evil-leader/set-key "e" 'eval-buffer)

(evil-leader/set-key "g" 'magit-status)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-mode 1)

