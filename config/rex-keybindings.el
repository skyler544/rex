;;; -*- lexical-binding: t -*-
;;
;; General is used to make keybinding simpler. This file has two
;; purposes: to set up the keybinding framework and to define keys
;; which are not related to external packages.
;;
;; Define leaders; the intention is for a tree-shaped layout of key
;; chords, starting with the spacebar and branching out to various
;; "leaves." Leaves may be single keys or further prefixes.
(use-package general :elpaca nil
  :ensure nil
  :config
  (general-override-mode)
  (general-create-definer rex-leader
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (rex-leader
    "m" '(:ignore t :which-key "local leader")
    "s" '(:ignore t :which-key "search")
    "c" '(:ignore t :which-key "code")
    "o" '(:ignore t :which-key "open")
    "e" '(:ignore t :which-key "eval")
    "g" '(:ignore t :which-key "goto")
    "p" '(:ignore t :which-key "project")
    "f" '(:ignore t :which-key "file")
    "t" '(:ignore t :which-key "toggle")
    "w" '(:ignore t :which-key "windows")
    "b" '(:ignore t :which-key "buffer")))

;; Miscellaneous keybindings
(use-package emacs :elpaca nil
  :general
  ("C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust
   "M-n" 'forward-list
   "M-p" 'backward-list
   "C-<tab>" 'indent-region)
  (rex-leader
    "SPC" 'execute-extended-command
    "." 'find-file
    "d" 'dired-jump
    "x" 'Control-X-prefix
    "Q" 'save-buffers-kill-emacs
    "qf" 'delete-frame
    "!" 'shell-command
    "&" 'async-shell-command
    "l" 'recenter-top-bottom
    "r" 'move-to-window-line-top-bottom
    "C" 'rex/async-shell-command-on-region-or-line
    "X" 'rex/shell-command-on-region-or-line
    ;; eval
    "eb" 'eval-buffer
    "er" 'eval-region
    "ef" 'eval-defun
    ;; open
    "of" 'make-frame
    "oe" 'eshell
    "om" 'man
    "oc" 'calendar
    ;; toggle
    "td" 'rex/dark-theme
    "tD" 'rex/light-theme
    "tt" 'toggle-truncate-lines
    "tw" 'visual-line-mode
    "tv" 'visible-mode
    "tL" 'display-line-numbers-mode
    ;; help
    "h" 'help-command
    "hF" 'describe-face
    "h'" 'describe-char
    "hp" 'elpaca-info
    ;; buffers
    "bz" 'bury-buffer
    "bR" 'rename-buffer
    "br" 'revert-buffer
    "bk" 'kill-this-buffer
    "bm" 'bookmark-set
    ;; files
    "fy" 'rex/kill-relative-path
    "fw" 'fixup-whitespace
    "fy" 'rex/kill-relative-path
    "fP" 'ffap
    "fs" 'save-buffer
    "fS" 'write-file
    "ff" 'find-file))

(use-package project :elpaca nil
  :general
  (rex-leader
    "p" project-prefix-map))

(use-package ibuffer :elpaca nil
  :general
  (rex-leader
    "bi" 'ibuffer))

(use-package winner :elpaca nil
  :demand t
  :config
  (winner-mode)
  :general
  (rex-leader
    :keymaps 'winner-mode-map
    "wr" 'winner-redo
    "wu" 'winner-undo))

(use-package info :elpaca nil
  :general
  ( :states 'normal
    :keymaps 'Info-mode-map
    "n" 'Info-forward-node
    "p" 'Info-backward-node))
