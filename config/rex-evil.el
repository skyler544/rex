;; The dark side.
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  (setq evil-move-beyond-eol t)
  (setq evil-respect-visual-line-mode t)
  :config
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
  :bind (:map evil-visual-state-map
	      ("s" . evil-surround-region)
	      ("S" . evil-Surround-region))
  :config
  (global-evil-surround-mode 1))
