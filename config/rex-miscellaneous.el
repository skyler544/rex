;;; -*- lexical-binding: t -*-
;;
;; General-purpose packages
;; ----------------------------------------------------
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
(use-package treemacs-evil)

;; Show previous versions of a file
(use-package git-timemachine
  :commands (git-timemachine))

;; Display git blame for the current line
(use-package why-this
  :general
  (rex-leader
    "tb" 'why-this-mode)
  :custom-face
  (why-this-face
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face
         :slant normal)))))

;; Highlight `TODO' items in buffers
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))


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
  :defer t
  :hook (rfc-mode . page-break-lines-mode))

;; Better pdf support
(use-package pdf-tools
  :vc (:fetcher github :repo "vedang/pdf-tools")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)

  (defun rex/ignore-errors (&rest r)
    (ignore-errors (apply (car r) (cdr r))))

  (advice-add 'pdf-view-goto-page :around #'rex/ignore-errors)

  :general
  (:keymaps 'pdf-view-mode-map
            "M-m" 'pdf-view-themed-minor-mode)
  :hook
  (pdf-view-mode . (lambda () (auto-composition-mode -1)))
  (pdf-view-mode . pdf-view-themed-minor-mode))

(use-package saveplace-pdf-view
  :after pdf-tools)


;; Web tinkering
;; ----------------------------------------------------
;; Program for turning a buffer into a webpage
(use-package htmlize)

;; Serve a buffer via http
(use-package impatient-mode)

(use-package restclient
  :mode ("\\.http" . restclient-mode))
