;;; -*- lexical-binding: t -*-
;;

;; Show key hints onscreen; nice for getting to know a new mode,
;; somewhat distracting after a while. The delay can be toggled back
;; to 10000 if needed.
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-add-column-padding 1)
  (setq which-key-side-window-slot -10)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; Better versions of some of the help functions. Add them to the C-h
;; map so that the entire prefix can be added to the general keybind
;; tree.
(use-package helpful
  :general
  ("C-h s" 'helpful-symbol)
  ("C-h f" 'helpful-callable)
  ("C-h k" 'helpful-key)
  ("C-h v" 'helpful-variable))

;; Colorize delimiters so that they indicate nesting depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; TODO Set this up; should make it possible to register commands and
;; select them via completion
(use-package run-command)

;; Terminal emulation
(use-package vterm
  :general
  (rex-leader
    "ot" 'vterm)
  :commands vterm)

(use-package eat
  :elpaca (:host codeberg :repo "akib/emacs-eat" :files (:defaults "./*")))

(use-package docker
  :general
  (rex-leader
    "mD" 'docker))

;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions. TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :general
  (:states 'normal
           "f" 'avy-goto-char-timer))

;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "bK" 'crux-kill-other-buffers
    "fY" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer
    "fr" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:states '(normal visual)
           "?" 'iedit-mode))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

;; Control temporary windows programmatically
(use-package popper
  :demand t
  :init
  (setq popper-mode-line
        (propertize " ▼ " 'face 'mode-line-emphasis))
  (setq popper-echo-mode t)
  (setq popper-window-height 0.4)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*help.*\\*" help-mode
          "\\*Calendar\\*"
          ("\\*Org Links\\*" . hide)
          "\\*Apropos\\*"
          "\\*Process List\\*"
          "\\*.*docker.*\\*"
          "\\*eldoc\\*" eldoc-mode
          "\\*vterm\\*" vterm-mode
          "\\*eat\\*" eat-mode
          "\\*eshell\\*" eshell-mode
          comint-mode
          ("\\*Async Shell Command\\*" . hide)
          compilation-mode))
  :general
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tk" 'popper-kill-latest-popup
    "tp" 'popper-toggle-type)
  :config
  (popper-mode))

;; Show buffers grouped by project in ibuffer
(use-package ibuffer-project
  :config
  (setq ibuffer-project-use-cache t)
  (defun rex/enable-ibuffer-project ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative)))
  :hook
  (ibuffer . rex/enable-ibuffer-project))

;; center buffer content in a window
(use-package olivetti
  :init
  (setq olivetti-body-width 0.8)
  :general
  (rex-leader
    "to" 'olivetti-mode))

(use-package expand-region
  :general
  (rex-leader
    "=" 'er/expand-region
    "-" 'er/contract-region))

;; allows you to increase font size globally
(use-package default-text-scale
  :config (default-text-scale-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; this shows long directory paths with a single file in them on one line
(use-package dired-collapse
  :elpaca (:host github :repo "Fuco1/dired-hacks")
  :hook (dired-mode . dired-collapse-mode))

;; tree-like directory navigation; very slow, but it looks cool
(use-package dired-subtree
  :elpaca (:host github :repo "Fuco1/dired-hacks")
  :config
  (setq rex/dired-subtree-levels
        '(dired-subtree-depth-1-face dired-subtree-depth-2-face
          dired-subtree-depth-3-face dired-subtree-depth-4-face
          dired-subtree-depth-5-face dired-subtree-depth-6-face))
  (dolist (face rex/dired-subtree-levels)
    (set-face-attribute face nil :extend t))
  :general
  (:states 'normal
   :keymaps 'dired-mode-map
   "o" 'dired-subtree-cycle))

(use-package treemacs-all-the-icons)

(use-package treemacs
  :init
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  :general
  (rex-leader
    "tT" 'treemacs)
  :config
  (setq treemacs-wrap-around nil)
  :commands 'treemacs)

(use-package treemacs-evil)

(use-package hide-mode-line)

(use-package page-break-lines)

(use-package whitespace-mode :elpaca nil
  :ensure nil
  :general
  (rex-leader
    "tW" 'whitespace-mode))

(use-package rfc-mode
  :defer t
  :hook (rfc-mode . page-break-lines-mode))

(use-package deadgrep
  :general
  (rex-leader
    "sD" 'deadgrep))

(use-package wgrep-deadgrep)
