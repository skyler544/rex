;;; -*- lexical-binding: t -*-
;;
;; Show key hints onscreen; nice for getting to know a new mode,
;; somewhat distracting after a while. The delay can be toggled back
;; to 10000 if needed.
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-add-column-padding 1)
  (setq which-key-side-window-slot -10)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; Better versions of some of the help functions. Add them to the C-h
;; map so that the entire prefix can be added to the general keybind
;; tree.
(use-package helpful
  :general
  ("C-h s" 'helpful-symbol)
  ("C-h f" 'helpful-callable)
  ("C-h k" 'helpful-key)
  ("C-h v" 'helpful-variable))

;; TODO Set this up; should make it possible to register commands and
;; select them via completion
(use-package run-command)

;; Terminal emulation
(use-package vterm
  :general
  (rex-leader
    "ot" 'vterm)
  :commands vterm)

(use-package docker
  :general
  (rex-leader
    "mD" 'docker))

(use-package dash)

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
    "bK" 'crux-kill-other-buffers
    "fy" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer
    "fr" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:keymaps '(normal visual)
           "?" 'iedit-mode))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package page-break-lines)

(use-package rfc-mode
  :defer t
  :hook (rfc-mode . page-break-lines-mode))

(use-package image-dired
  :elpaca nil
  :ensure nil
  :defer t)

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

(use-package esup
  :defer t)
