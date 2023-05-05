;;; -*- lexical-binding: t -*-
;;
;; The dark side.
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  (setq evil-move-beyond-eol t)
  (setq evil-respect-visual-line-mode t)

  :config
  (setq evil-mode-line-format '(before . moody-mode-line-buffer-identification))
  (evil-set-initial-state 'org-agenda-mode 'normal)

  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-mode)

  :general
  (rex-leader
    "wH" 'rex/window-move-left
    "wJ" 'rex/window-move-down
    "wK" 'rex/window-move-up
    "wL" 'rex/window-move-right)
  (:keymaps '(normal visual)
            "C-w" 'evil-scroll-line-up)
  (rex-leader
    "w" 'evil-window-map
    "wr" 'nil
    "ws" 'rex/split-and-follow-horizontally
    "wv" 'rex/split-and-follow-vertically)
  (:keymaps 'evil-inner-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  (:keymaps 'evil-outer-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  (:keymaps 'insert
            "C-k" 'ignore)
  :hook (git-commit-mode . evil-insert-state))

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
  (:keymaps 'visual
           "s" 'evil-surround-region
           "S" 'evil-Surround-region)
  (:keymaps 'operator
            "s" 'evil-surround-edit)
  (rex-leader
    "bn" 'evil-buffer-new)
  :config
  (global-evil-surround-mode 1))

;; Add keybindings for changing inner/outer blocks/quotes.
(use-package evil-textobj-anyblock
  :config
  (evil-define-text-object rex/evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest inner quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))
  (evil-define-text-object rex/evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))
  :general
  (:keymaps 'evil-inner-text-objects-map
            "q" 'rex/evil-textobj-anyblock-inner-quote
            "b" 'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
            "q" 'rex/evil-textobj-anyblock-a-quote
            "b" 'evil-textobj-anyblock-a-block))

(defun evil-record-macro ()
  "NoOp -- there must be a better way of disabling this."
  (interactive)
  (ignore))

(use-package evil-lion
  :config (evil-lion-mode))

(use-package evil-nerd-commenter
  :general
  (:keymaps '(normal visual)
            "gcc" 'evilnc-comment-or-uncomment-lines))
