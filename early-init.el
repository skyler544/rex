;;; -*- lexical-binding: t -*-
;;
;; Basic initialization
;; ----------------------------------------------------
;; Redirect native-comp cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache" user-emacs-directory))))

;; If something goes wrong, try to fix it interactively.
(setq debug-on-error t)

;; Just don't.
(setq custom-file "/dev/null")


;; Package management
;; ----------------------------------------------------
;; Start up the built-in Emacs package manager.
(package-initialize)

;; Add a larger package repository.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; If the package list is empty, initialize it.
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package and the :vc extension
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))


;; Garbage collection
;; ----------------------------------------------------
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-high-cons-threshold #x1000000)) ; 16MB
