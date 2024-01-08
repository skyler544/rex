;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Keybinding management
;; ----------------------------------------------------


;; Keymap tree with SPC as leader -- general.el
;; ----------------------------------------------------
(use-package general
  :config
  ;; base options
  (general-auto-unbind-keys)
  (general-evil-setup t)
  (setq evil-want-keybinding nil)

  ;; leader setup
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

(use-package emacs
  :general
  ("C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust)
  (rex-leader
    "SPC" 'execute-extended-command
    "." 'find-file
    "d" 'dired-jump
    "x" 'Control-X-prefix
    "Q" 'save-buffers-kill-emacs
    "qf" 'delete-frame
    "!" 'shell-command
    "&" 'async-shell-command
    "l" 'recenter-top-bottom
    "r" 'move-to-window-line-top-bottom
    "C" 'rex/async-shell-command-on-region-or-line
    "X" 'rex/shell-command-on-region-or-line
    ;; eval
    "eb" 'eval-buffer
    "er" 'eval-region
    "ef" 'eval-defun
    ;; open
    "of" 'make-frame
    "om" 'man
    "oc" 'calendar
	;; projects
    "p" project-prefix-map
    ;; toggle
    "td" 'rex/dark-theme
    "tD" 'rex/light-theme
    "tt" 'toggle-truncate-lines
    "tw" 'visual-line-mode
    "tW" 'whitespace-mode
    "tv" 'visible-mode
    "tL" 'display-line-numbers-mode
    ;; help
    "h" 'help-command
    "hF" 'describe-face
    "h'" 'describe-char
    ;; buffers
    "bi" 'ibuffer
    "bz" 'bury-buffer
    "bR" 'rename-buffer
    "br" 'revert-buffer
    "bk" 'kill-this-buffer
    "bm" 'bookmark-set
    ;; files
    "fy" 'rex/kill-relative-path
    "fw" 'fixup-whitespace
    "fP" 'ffap
    "fs" 'save-buffer
    "fS" 'write-file
    "ff" 'find-file))

(use-package info
  :config
  ;; HACK: This replaces a lambda hook with `evil-local-set-key'
  (define-minor-mode rex/evil-info-mode
    "Somehow lets you force general to bind keys that it otherwise refuses to bind.")
  :hook (Info-mode . rex/evil-info-mode)
  :general
  ( :states 'normal
    :definer 'minor-mode
    :keymaps 'rex/evil-info-mode
    "n" 'Info-forward-node
    "p" 'Info-backward-node
    "C-j" 'Info-scroll-up
    "C-k" 'Info-scroll-down))


;; Modal editing
;; ----------------------------------------------------
(use-package evil
  :after general
  :init
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  (setq evil-move-beyond-eol t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode)
  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))
  :general
  (rex-leader
    "bn" 'evil-buffer-new
    ;; windows
    "w" 'evil-window-map
    "wH" 'rex/window-move-left
    "wJ" 'rex/window-move-down
    "wK" 'rex/window-move-up
    "wL" 'rex/window-move-right
    "wu" 'winner-undo
    "wr" 'winner-redo
    "ws" 'rex/split-and-follow-horizontally
    "wv" 'rex/split-and-follow-vertically)
  (:states 'normal
           "q" 'ignore)
  (:states '(normal visual)
           "C-w" 'evil-scroll-line-up
           "C-j" 'evil-scroll-page-down
           "C-k" 'evil-scroll-page-up
           "C-d" 'delete-char
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-f" 'forward-char
           "C-b" 'backward-char)
  (:keymaps 'evil-inner-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  (:keymaps 'evil-outer-text-objects-map
            "g" '+evil:whole-buffer-txtobj)
  (:keymaps 'insert
            "C-k" 'ignore)
  :hook (git-commit-mode . evil-insert-state))

(use-package evil-collection
  :config
  (with-eval-after-load 'evil
   (require 'evil-collection)
   (setq evil-collection-mode-list (delq 'lispy evil-collection-mode-list))
   (evil-collection-init)))

;; Show a brief flash in the buffer indicating where an evil
;; operation takes effect.
(use-package evil-goggles
  :custom-face
  (evil-goggles-default-face
   ((t (:background unspecified :inherit lazy-highlight))))
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

(use-package evil-lion
  :config (evil-lion-mode))

(use-package evil-commentary
  :config (evil-commentary-mode))

(use-package evil-mc
  :config (global-evil-mc-mode))
