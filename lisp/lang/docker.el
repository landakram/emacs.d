;;; lang/docker.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


(use-package dockerfile-mode
  :ensure t)

(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
(setenv "DOCKER_CERT_PATH" "/Users/mark/.docker/machine/machines/default")
(setenv "DOCKER_MACHINE_NAME" "default")


(use-package docker
  :ensure t
  :defer t
  :diminish docker-mode)

(provide 'lang/docker)
;;; lang/docker.el ends here
