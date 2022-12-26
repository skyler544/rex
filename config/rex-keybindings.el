;; Define leaders; the intention is for a tree-shaped layout of key
;; chords, starting with the spacebar and branching out to various
;; "leaves." Leaves may be single keys or further prefixes.
(use-package general
  :config
  (general-create-definer rex-leader
    :keymaps '(visual normal)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer rex-local-leader
    :keymaps '(visual normal)
    :prefix "SPC m")
  (general-create-definer rex-search-leader
    :keymaps '(visual normal)
    :prefix "SPC s")
  (general-create-definer rex-goto-leader
    :keymaps '(visual normal)
    :prefix "SPC g")
  (general-create-definer rex-file-leader
    :keymaps '(visual normal)
    :prefix "SPC f")
  (general-create-definer rex-buffer-leader
    :keymaps '(visual normal)
    :prefix "SPC b"))

;; Miscellaneous keybindings
(use-package emacs
  :general
  (rex-leader
    "." 'find-file
    "x" 'Control-X-prefix
    "h" 'help-command)
  (rex-buffer-leader
    "k" 'kill-this-buffer)
  (rex-file-leader
    "s" 'save-buffer
    "f" 'find-file))

(use-package project
  :ensure nil
  :general
  (rex-leader
    :prefix "SPC p"
    :prefix-map 'project-prefix-map))
