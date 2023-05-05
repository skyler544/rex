;;; -*- lexical-binding: t -*-
;;
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

(use-package ibuffer-project
  :config
  (setq ibuffer-project-use-cache t)
  (defun rex/enable-ibuffer-project ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative)))
  :hook
  (ibuffer . rex/enable-ibuffer-project))

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

(use-package hide-mode-line)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
