;;; -*- lexical-binding: t -*-
;;
;; Set the font and load a theme. Also disable unused GUI elements.
(use-package emacs
  :init
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (set-fringe-mode 4)

  (setq default-frame-alist
        (append (list '(font . "Iosevka Custom Extended:size=20"))))

  (set-face-attribute 'variable-pitch nil
                    :family "Iosevka Custom Extended"
                    :slant 'oblique))

(use-package emacs
  :config
  (setq custom-theme-directory (concat user-emacs-directory "themes/")))

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

;; Hide the long details by default
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

;; Show the results of C-x C-e directly in the buffer
(use-package eros
  :config
  (eros-mode 1))

;; Briefly flash current line after a long movement.
(use-package pulsar
  :after evil
  :general
  (rex-leader
    "C-SPC" 'pulsar-highlight-line)
  :init
  (setq pulsar-face 'pulsar-generic)
  :config

  (setq pulsar-functions
        '(evil-window-down
          evil-window-up
          avy-goto-char-timer))
  (dolist (fkt pulsar-functions)
    (add-to-list 'pulsar-pulse-functions fkt))

  (face-spec-reset-face 'pulsar-generic)
  (set-face-attribute 'pulsar-generic nil :inherit 'region)

  (pulsar-global-mode))

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode
  :commands 'rainbow-mode)

;; Redefine the fringe arrows.
(use-package emacs
  :config
  (define-fringe-bitmap 'right-arrow
    [#b00001110
     #b00001110
     #b00001110
     #b11111110
     #b11111110
     #b00001110
     #b00001110
     #b00001110])
  (define-fringe-bitmap 'left-arrow
    [#b01110000
     #b01110000
     #b01110000
     #b01111111
     #b01111111
     #b01110000
     #b01110000
     #b01110000])
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b01111100
     #b01111100
     #b00001100
     #b00001100
     #b00001100
     #b00001100])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00110000
     #b00110000
     #b00110000
     #b00110000
     #b00111110
     #b00111110
     #b00000000
     #b00000000]))
