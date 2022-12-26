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
    "m" '(:ignore t :which-key "local leader")
    "s" '(:ignore t :which-key "search")
    "g" '(:ignore t :which-key "goto")
    "f" '(:ignore t :which-key "file")
    "w" '(:ignore t :which-key "windows")
    "b" '(:ignore t :which-key "buffer")))

;; Miscellaneous keybindings
(use-package emacs
  :general
  (rex-leader
    "." 'find-file
    "x" 'Control-X-prefix
    "h" 'help-command
    ;; buffers
    "br" 'revert-buffer
    "bk" 'kill-this-buffer
    ;; files
    "fs" 'save-buffer
    "fS" 'write-file
    "ff" 'find-file))

;; (use-package dired
;;   :ensure nil
;;   :general
;;   (:keymaps 'dired-mode-map
;;             :prefix "SPC"))

(use-package project
  :ensure nil
  :general
  (rex-leader
    "p" 'project-prefix-map))
