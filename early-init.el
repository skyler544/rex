;;; -*- lexical-binding: t -*-
;;
;; This file initializes the package system and installs use-package.
;; diminish and general are also initialized here as they are widely
;; used throughout rex. Frame related setup is also performed here.
;;
;; This file should not be modified frequently (if at all).

(setq package-enable-at-startup nil)
(setq debug-on-error t)

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(load (concat user-emacs-directory "elpaca-loader"))

(use-package use-package
  :elpaca nil
  :config
  (setq use-package-always-ensure t))

(use-package emacs
  :elpaca nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq frame-resize-pixelwise t)
  (setq custom-theme-directory (concat user-emacs-directory "themes/"))
  :custom-face
  (whitespace-newline ((t (:foreground nil :inherit font-lock-warning-face))))
  (whitespace-space ((t (:foreground nil :inherit font-lock-warning-face))))
  (fringe ((t (:background nil))))
  (variable-pitch ((t (:family "monospace"))))
  (help-key-binding
   ((t (:foreground nil :background nil :box nil :inverse-video t)))))

;; Adds a keyword to use-package that makes hiding minor modes from the modeline
;; simple
(use-package diminish :demand t)

;; general makes defining keys (particularly when using leader keys)
;; much simpler.
(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup t))

(elpaca-wait)
