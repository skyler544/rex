;;; -*- lexical-binding: t -*-
;;
;; Set the font and load a theme. Also disable unused GUI elements.
(use-package emacs
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-fringe-mode 4)
  (setq default-frame-alist
        (append (list '(font . "Iosevka Custom Extended:size=20"))))
  (set-face-attribute 'variable-pitch nil
                    :family "Iosevka Custom Extended"
                    :slant 'oblique)
  (set-face-attribute 'help-key-binding nil
                      :background 'nil
                      ;; :foreground (face-foreground 'font-lock-builtin-face)
                      :foreground 'nil
                      :weight 'bold
                      :box '(:line-width -1)))

(use-package info
  :ensure nil
  :config
  (set-face-attribute 'Info-quoted nil :inherit 'nil)
  (set-face-attribute 'Info-quoted nil :inherit 'font-lock-function-name-face))


;; Huge theme pack 
(use-package doom-themes
  :config
  (load-theme 'doom-nord-aurora t)
  (set-face-attribute 'line-number nil
                      :slant 'normal)
  (set-face-attribute 'line-number-current-line nil
                      :slant 'normal))

;; Flashy modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 38)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

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
  :demand t
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
          evil-window-right
          evil-window-left
          evil-avy-goto-char-timer
          dired-jump
          other-window
          isearch-repeat-forward
          isearch-repeat-backward))
  (dolist (fkt pulsar-functions)
    (add-to-list 'pulsar-pulse-functions fkt))
  (face-spec-reset-face 'pulsar-generic)
  (set-face-attribute 'pulsar-generic nil :inherit 'region)
  (pulsar-global-mode))

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode
  :commands 'rainbow-mode)

;; better pdf support
(use-package pdf-tools
  :load-path "~/build/pdf-tools/lisp"
  :diminish Pdf-View-Midnight
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width))

(use-package image-roll
  :load-path "~/build/image-roll.el/"
  :init (require 'image-roll)
  :after pdf-view
  :hook (pdf-view-mode . pdf-view-roll-minor-mode))
