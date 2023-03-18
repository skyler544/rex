;;; -*- lexical-binding: t -*-
;;
;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions.  TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :general
  (:keymaps 'normal
           "f" 'avy-goto-char-timer))

;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "bK" 'crux-kill-other-buffers
    "fy" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer
    "fr" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:keymaps '(normal visual)
           "?" 'iedit-mode))

;; Control temporary windows programmatically
(use-package popper
  :demand t
  :general
  (:keymaps '(normal visual)
            "C-`" 'popper-toggle-latest
            "M-`" 'popper-cycle
            "C-M-`" 'popper-toggle-type)
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tk" 'popper-kill-latest-popup
    "tp" 'popper-toggle-type)
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
          "\\*eshell\\*" eshell-mode))
  :config
  (popper-mode))

(use-package ibuffer-project
  :config
  (setq ibuffer-project-cache (concat rex/cache-dir "ibuffer-project-cache"))
  (setq ibuffer-project-use-cache t)
  :hook
  (ibuffer . (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative)))))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package olivetti
  :general
  (rex-leader
    "to" 'olivetti-mode))

(use-package expand-region
  :general
  (rex-leader
    "=" 'er/expand-region
    "-" 'er/contract-region))

(use-package default-text-scale
  :config (default-text-scale-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-collapse
  :load-path "~/build/dired-hacks/"
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-subtree
  :load-path "~/build/dired-hacks/"
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
