;;; -*- lexical-binding: t -*-
;;
;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions.  TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :general
  (:keymaps 'normal
           "f" 'avy-goto-char-timer))

;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "fy" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer
    "fr" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:keymaps '(normal visual)
           "?" 'iedit-mode))

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

;; Terminal emulation
(use-package vterm
  :commands vterm)

(use-package eat)

;; Control temporary windows programmatically
(use-package popper
  :general
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tp" 'popper-toggle-type)
  :init
  (setq popper-window-height 0.25)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*compilation\\*"
          "Output\\*$"
          "\\*helpful.*\\*"
          "\\*eldoc\\*" eldoc-mode
          "\\*vterm\\*" vterm-mode
          "\\*eat\\*" eat-mode
          ("\\*Async Shell Command\\*" . hide)
          help-mode
          compilation-mode))
  :config
  (popper-mode))
