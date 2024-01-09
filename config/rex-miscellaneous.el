;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Miscellaneous packages
;; ----------------------------------------------------


;; General-purpose packages
;; ----------------------------------------------------
;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "fY" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer))

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode)

;; Highlight `TODO' items in buffers
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Hiding buffer content stylishly
(use-package redacted)

;; Hide the mode line temporarily
(use-package hide-mode-line)

;; Tree-style project drawer
(use-package treemacs-all-the-icons)
(use-package treemacs
  :init
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  :general
  (rex-leader
    "tT" 'treemacs)
  :config
  (setq treemacs-user-mode-line-format 'none)
  (setq treemacs-wrap-around nil)
  (setq treemacs-wide-toggle-width 50)
  :commands 'treemacs)
(use-package treemacs-evil
  :config
  (evil-set-initial-state 'treemacs-mode 'normal))


;; Reading and prose
;; ----------------------------------------------------
(use-package olivetti
  :init
  (setq olivetti-body-width 0.8)
  :general
  (rex-leader
    "to" 'olivetti-mode))

(use-package copy-as-format)

;; Read RFCs
(use-package page-break-lines)
(use-package rfc-mode
  :hook
  (help-mode . page-break-lines-mode)
  (rfc-mode . page-break-lines-mode))

;; Better pdf support
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)

  (defun rex/ignore-errors (&rest r)
    (ignore-errors (apply (car r) (cdr r))))

  (advice-add 'pdf-view-goto-page :around #'rex/ignore-errors)
  :hook
  (pdf-view-mode . (lambda () (auto-composition-mode -1)))
  (pdf-view-mode . pdf-view-midnight-minor-mode))

(use-package saveplace-pdf-view
  :after pdf-tools)

(use-package dictcc)


;; Web tinkering
;; ----------------------------------------------------
;; Program for turning a buffer into a webpage
(use-package htmlize)

;; Serve a buffer via http
(use-package impatient-mode)

(use-package restclient
  :mode ("\\.http" . restclient-mode))

(use-package grip-mode
  :config
  (setq grip-github-user (getenv "GH_API_USER"))
  (setq grip-github-password (getenv "GH_API_TOKEN")))
