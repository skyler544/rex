(use-package vertico
  :init
  (vertico-mode)
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
  :config
  (setq vertico-count 12)
  (setq vertico-resize nil))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :general
  (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char
            "M-DEL" 'vertico-directory-delete-word)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file) (styles partial-completion))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :general
  ("C-," 'embark-act)
  ("C-h B" 'embark-bindings)
  :config
  (setq embark-indicators '(embark-minimal-indicator))
  (setq embark-prompter 'embark-completing-read-prompter))

(use-package consult
  :general
  ("P" 'consult-yank-pop)
  ("C-x b" 'consult-buffer)
  (:keymaps 'vertico-map
    "M-s" 'consult-history)
  (rex-leader
    ":" 'consult-complex-command
    "," 'consult-buffer)
  (rex-goto-leader
    "g" 'consult-goto-line
    "m" 'consult-mark
    "k" 'consult-global-mark)
  (rex-search-leader
    "d" 'consult-ripgrep
    "i" 'consult-imenu
    "I" 'consult-imenu-multi
    "l" 'consult-line
    "e" 'consult-isearch-history)
  (rex-local-leader
    "m" 'consult-mode-command))

(use-package consult-dir
  :general
  (:keymaps 'vertico-map
            "C-x C-d" 'consult-dir))
