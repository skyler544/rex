;;; -*- lexical-binding: t -*-
;;
;; Set the font and load a theme. Also disable unused GUI elements.
(use-package emacs
  :init
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (setq default-frame-alist
        (append (list '(font . "Iosevka Custom Extended:size=20"))))

  (set-face-attribute 'variable-pitch nil
                    :family "Iosevka Custom Extended"
                    :slant 'oblique))

(use-package emacs
  :config
  (setq custom-theme-directory (concat user-emacs-directory "themes/")))

(use-package ef-themes)

;; Manual installation of this theme to allow editing the modeline
;; without modifying the package inside the elpa folder.
;; https://github.com/emacsfodder/emacs-theme-creamsody
(use-package autothemer)
(use-package creamsody-theme
  :load-path "themes/emacs-theme-creamsody/"
  :init (require 'creamsody)
  :config
  (load-theme 'creamsody t)
  (creamsody-modeline))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (set-face-attribute 'all-the-icons-dired-dir-face nil
                      :foreground nil
                      :inherit 'font-lock-type-face))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package eros
  :config
  (eros-mode 1))
