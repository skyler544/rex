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
  :general
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
  :config
  (setq evil-mode-line-format '(before . moody-mode-line-buffer-identification))
  (evil-set-initial-state 'org-agenda-mode 'normal)
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
  "Noop -- there must be a better way of disabling this."
  (interactive)
  (ignore))

(use-package evil-lion
  :config (evil-lion-mode))

;; window movement; stolen from doom
(use-package evil
  :config
  (defun rex--window-swap (direction)
    "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
    (when (window-dedicated-p)
      (user-error "Cannot swap a dedicated window"))
    (let* ((this-window (selected-window))
           (this-buffer (current-buffer))
           (that-window (windmove-find-other-window direction nil this-window))
           (that-buffer (window-buffer that-window)))
      (when (or (minibufferp that-buffer)
                (window-dedicated-p this-window))
        (setq that-buffer nil that-window nil))
      (if (not (or that-window (one-window-p t)))
          (funcall (pcase direction
                     ('left  #'evil-window-move-far-left)
                     ('right #'evil-window-move-far-right)
                     ('up    #'evil-window-move-very-top)
                     ('down  #'evil-window-move-very-bottom)))
        (unless that-window
          (setq that-window
                (split-window this-window nil
                              (pcase direction
                                ('up 'above)
                                ('down 'below)
                                (_ direction))))
          (setq that-buffer (window-buffer that-window)))
        (window-swap-states this-window that-window)
        (select-window that-window))))
  (defun rex/window-move-left ()
    "Swap windows to the left."
    (interactive) (rex--window-swap 'left))
  (defun rex/window-move-down ()
    "Swap windows downward."
    (interactive) (rex--window-swap 'down))
  (defun rex/window-move-up ()
    "Swap windows upward."
    (interactive) (rex--window-swap 'up))
  (defun rex/window-move-right ()
    "Swap windows to the right"
    (interactive) (rex--window-swap 'right))
  :general
  (rex-leader
    "wH" 'rex/window-move-left
    "wJ" 'rex/window-move-down
    "wK" 'rex/window-move-up
    "wL" 'rex/window-move-right))
