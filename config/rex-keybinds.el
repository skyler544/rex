;;; -*- lexical-binding: t -*-
;;
;; Keybinding management
;; ----------------------------------------------------
;; general makes defining keys simpler.
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
    "b" '(:ignore t :which-key "buffer"))

  ;; must be enabled by general
  (evil-mode))

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
    "ff" 'find-file
    ;; windows
    "wH" 'rex/window-move-left
    "wJ" 'rex/window-move-down
    "wK" 'rex/window-move-up
    "wL" 'rex/window-move-right
    "wr" 'nil
    "ws" 'rex/split-and-follow-horizontally
    "wv" 'rex/split-and-follow-vertically))

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
  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))
  :general
  (rex-leader
    "w" 'evil-window-map
    "wr" 'nil)
  (:states 'normal
           "q" 'ignore)
  (:states '(normal visual)
           "C-w" 'evil-scroll-line-up)
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
;; operation takes effect.
(use-package evil-goggles
  :diminish evil-goggles-mode
  :custom-face
  (evil-goggles-default-face
   ((t (:background nil :inherit lazy-highlight))))
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

(use-package evil-lion
  :config (evil-lion-mode))

(use-package evil-commentary
  :diminish
  :config
  (evil-commentary-mode))

(use-package evil-mc
  :config
  (global-evil-mc-mode))
