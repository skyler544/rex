;; The dark side.
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  (setq evil-move-beyond-eol t)
  (setq evil-respect-visual-line-mode t)
  :general
  (rex-leader
    :prefix "SPC w"
    :prefix-map 'evil-window-map)
  (:keymaps 'evil-inner-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  (:keymaps 'evil-outer-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  :config
  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (evil-mode))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; Show a brief flash in the buffer indicating where an evil
;; operation takes affect.
(use-package evil-goggles
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode))

;; Make adding a pair of delimiters to the text fast and simple.
(use-package evil-surround
  :general
  (:states 'visual
           "s" 'evil-surround-region
           "S" 'evil-Surround-region)
  (:states 'operator
            "s" 'evil-surround-edit)
  :config
  (global-evil-surround-mode 1))

;; Add keybindings for changing inner/outer blocks/quotes.
(use-package evil-textobj-anyblock
  :config
  :general
  (:keymaps 'evil-inner-text-objects-map
            "b" 'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
            "b" 'evil-textobj-anyblock-a-block))
