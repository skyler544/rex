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

(use-package emacs :elpaca nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq use-package-always-ensure t)
  (setq frame-resize-pixelwise t)
  (setq custom-theme-directory (concat user-emacs-directory "themes/"))
  :custom-face
  (whitespace-newline
   ((t ( :foreground unspecified
         :inherit font-lock-warning-face))))
  (whitespace-space
   ((t ( :foreground unspecified
         :inherit font-lock-warning-face))))
  (fringe
   ((t ( :background unspecified))))
  (variable-pitch
   ((t ( :family "monospace"))))
  (shadow
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face))))
  (help-key-binding
   ((t ( :foreground unspecified
         :background unspecified
         :weight bold
         :box unspecified
         :inverse-video t)))))

;; Adds a keyword to use-package for hiding minor modes from the modeline
(use-package diminish
  :demand t)

;; general makes defining keys (particularly when using leader keys) much
;; simpler. Installing it here and configuring it later keeps this file simpler.
(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup t))

;; Necessary after installing packages that add keywords to use-package
(elpaca-wait)
