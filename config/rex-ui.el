;;; -*- lexical-binding: t -*-
;;
;; Set the font and load a theme. Also disable unused GUI elements.
(use-package emacs
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  :custom-face
  (variable-pitch ((t (:family "monospace"))))
  (help-key-binding
   ((t (:foreground nil :background nil :box nil :inverse-video t)))))

(use-package info
  :ensure nil
  :custom-face
  (info-menu-star ((t (:foreground nil))))
  (Info-quoted
   ((t (:inherit nil :inherit font-lock-function-name-face)))))

;; Huge theme pack 
(use-package doom-themes
  :custom-face
  (line-number
   ((t (:inherit nil :background nil :slant normal))))
  (line-number-current-line
   ((t (:inherit nil :background nil :slant normal))))
  :config
  (load-theme 'doom-nord-aurora t))

;; Flashy modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 38)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom-face
  (all-the-icons-dired-dir-face
   ((t (:foreground nil :inherit font-lock-type-face)))))

;; Show the results of C-x C-e directly in the buffer
(use-package eros
  :config
  (eros-mode 1))

;; Briefly flash current line after a long movement.
(use-package pulsar
  :demand t
  :after evil
  :custom-face
  (pulsar-generic ((t (:background nil :inherit region))))
  :general
  (rex-leader
    "C-SPC" 'pulsar-highlight-line)
  :init
  (setq pulsar-face 'pulsar-generic)
  :config
  (setq pulsar-functions
        '(evil-window-down
          evil-window-up
          evil-window-right
          evil-window-left
          evil-avy-goto-char-timer
          dired-jump
          other-window
          isearch-repeat-forward
          isearch-repeat-backward))
  (dolist (fkt pulsar-functions)
    (add-to-list 'pulsar-pulse-functions fkt))
  (pulsar-global-mode))

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode
  :commands 'rainbow-mode)

;; better pdf support
(use-package pdf-tools
  :diminish pdf-view-midnight-minor-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :config (setq-default pdf-view-display-size 'fit-width))

(use-package saveplace-pdf-view
  :after pdf-tools)

;; Show diffs in the fringe.
(use-package diff-hl
  :custom-face
  (diff-hl-insert ((t (:background nil))))
  (diff-hl-change ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  :config
  (let* ((height (frame-char-height))
         (width 4)
         (ones (1- (expt 2 width)))
         (bits (make-vector height ones)))
    (define-fringe-bitmap 'rex/diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'rex/diff-hl-bitmap))
  :hook (prog-mode . diff-hl-mode))
