;;; -*- lexical-binding: t -*-
;;
;; Automatically closes pairs; works most of the time the right way
;; without needing to think about it.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

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

;; Makes creating 'keymap menus' simpler.
(use-package hydra
  :init
  (require 'windmove)
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))


  (defhydra +hydra/window-nav (:hint nil)
    "
Resize: _h_: left  _j_: down  _k_: up  _l_: right "
    ("h" hydra-move-splitter-left       )
    ("j" hydra-move-splitter-down       )
    ("k" hydra-move-splitter-up         )
    ("l" hydra-move-splitter-right      )
    ("q" nil                           ))
  :general
  (rex-leader
    "ww" '+hydra/window-nav/body))

;; Terminal emulation
(use-package vterm
  :commands vterm)

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
          "\\*vterm\\*" vterm-mode
          ("\\*Async Shell Command\\*" . hide)
          help-mode
          compilation-mode))
  :config
  (popper-mode))
