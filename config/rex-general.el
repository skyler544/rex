;; Define leaders
(use-package general
  :config
  (general-create-definer rex-leader
    :keymaps '(visual normal motion)
    :prefix "SPC")
  (general-create-definer rex-local-leader
    :keymaps '(visual normal motion)
    :prefix "SPC m")
  (general-create-definer rex-project-leader
    :keymaps '(visual normal motion)
    :prefix "SPC p")
  (general-create-definer rex-search-leader
    :keymaps '(visual normal motion)
    :prefix "SPC s")
  (general-create-definer rex-goto-leader
    :keymaps '(visual normal motion)
    :prefix "SPC g")
  (general-create-definer rex-file-leader
    :keymaps '(visual normal motion)
    :prefix "SPC f")
  (general-create-definer rex-buffer-leader
    :keymaps '(visual normal motion)
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
  (rex-project-leader
    "f" 'project-find-file))
