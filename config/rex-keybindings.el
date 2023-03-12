;;; -*- lexical-binding: t -*-
;;
;; General is used to make keybinding simpler. This file has two
;; purposes: to set up the keybinding framework and to define keys
;; which are not related to external packages.
;;
;; Define leaders; the intention is for a tree-shaped layout of key
;; chords, starting with the spacebar and branching out to various
;; "leaves." Leaves may be single keys or further prefixes.
(use-package general
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
(use-package emacs
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
    "!" 'shell-command
    "R" 'rex/shell-command-on-region-or-line
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
    "tt" 'toggle-truncate-lines
    "tw" 'toggle-word-wrap
    ;; help
    "h" 'help-command
    "hF" 'describe-face
    "h'" 'describe-char
    ;; buffers
    "bz" 'bury-buffer
    "br" 'revert-buffer
    "bk" 'kill-this-buffer
    "bm" 'bookmark-set
    ;; files
    "fw" 'fixup-whitespace
    "fP" 'ffap
    "fs" 'save-buffer
    "fS" 'write-file
    "ff" 'find-file))

(use-package project
  :ensure nil
  :general
  (rex-leader
    "p" project-prefix-map))

(use-package ibuffer
  :ensure nil
  :general
  (rex-leader
    "bi" 'ibuffer))

(use-package dired
  :ensure nil
  :general (:keymaps '(normal dired-mode-map)
                     "C-k" 'dired-kill-line))

(use-package winner
  :demand t
  :config
  (winner-mode)
  :general
  (rex-leader
    :keymaps 'winner-mode-map
    "wr" 'winner-redo
    "wu" 'winner-undo))
