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

;; Terminal emulation
(use-package vterm
  :commands vterm)

(use-package eat)

;; Control temporary windows programmatically
(use-package popper
  :demand t
  :general
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tk" 'popper-kill-latest-popup
    "tp" 'popper-toggle-type)
  :init
  (setq popper-window-height 0.3)
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
