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

(use-package docker-tramp)

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

(use-package image-dired
  :defer t)

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

(use-package esup
  :defer t)
