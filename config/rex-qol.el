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
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tk" 'popper-kill-latest-popup
    "tp" 'popper-toggle-type)
  :init
  (setq popper-mode-line
        (propertize " ▼ " 'face 'mode-line-emphasis))
  (setq popper-echo-mode t)
  (setq popper-window-height 0.35)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*compilation\\*"
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
