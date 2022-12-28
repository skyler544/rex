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
  (general-create-definer rex-leader
    :keymaps '(visual normal emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "C-SPC"
    "m" '(:ignore t :which-key "local leader")
    "s" '(:ignore t :which-key "search")
    "c" '(:ignore t :which-key "code")
    "g" '(:ignore t :which-key "goto")
    "p" '(:ignore t :which-key "project")
    "f" '(:ignore t :which-key "file")
    "w" '(:ignore t :which-key "windows")
    "b" '(:ignore t :which-key "buffer")))

;; Miscellaneous keybindings
(use-package emacs
  :general
  (rex-leader
    "SPC" 'execute-extended-command
    "." 'find-file
    "x" 'Control-X-prefix
    "Q" 'save-buffers-kill-emacs
    ;; help
    "h" 'help-command
    "hF" 'describe-face
    "h'" 'describe-char
    ;; buffers
    "bz" 'bury-buffer
    "br" 'revert-buffer
    "bk" 'kill-this-buffer
    ;; files
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
