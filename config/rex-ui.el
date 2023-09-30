;;; -*- lexical-binding: t -*-
;;
;; User Interface configuration
;; ----------------------------------------------------


;; Fonts
;; ----------------------------------------------------
(use-package emacs
  :config
  (set-face-attribute 'variable-pitch t :family "monospace")
  (setq *rex/face-size* 120)

  (defun rex/font-exists (fam)
    (if (null (x-list-fonts fam)) nil t))

  (defun rex/frame-init (face fam size)
    (set-face-attribute face nil
                        :family fam
                        :height size))

  (defun rex/frame-init-aux (fam size)
    (when (rex/font-exists fam)
      (let ((faces '(default fixed-pitch variable-pitch)))
        (dolist (face faces)
          (rex/frame-init face fam size)))))

  (defun rex/frame-init-iosevka ()
    (interactive)
    (rex/frame-init-aux "Iosevka Custom" *rex/face-size*))
  (defun rex/frame-init-commit ()
    (interactive)
    (rex/frame-init-aux "Commit Mono" *rex/face-size*))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (rex/frame-init-commit)))
    (rex/frame-init-commit)))


;; Themes
;; ----------------------------------------------------
(use-package emacs
  :config
  (setq inhibit-x-resources t)
  (setq custom-theme-directory *rex/theme-dir*))

(use-package doom-themes
  :custom-face
  (line-number
   ((t ( :slant normal))))
  (line-number-current-line
   ((t ( :inherit nil
         :inherit 'default
         :slant normal
         :weight semi-bold))))
  :config (load-theme 'doom-tango t))


;; Mode Line
;; ----------------------------------------------------
(use-package moody
  :config
  (setq evil-mode-line-format '(before . moody-mode-line-buffer-identification))
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 38)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))



;; Icons
;; ----------------------------------------------------
(use-package all-the-icons
  :config
  (setq all-the-icons-color-icons nil))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom-face
  (all-the-icons-dired-dir-face
   ((t ( :foreground unspecified
         :inherit font-lock-type-face)))))


;; Feedback
;; ----------------------------------------------------
;; Show the results of C-x C-e directly in the buffer
(use-package eros
  :config
  (setq eros-eval-result-prefix "-> ")
  (eros-mode 1)
  :custom-face
  (eros-result-overlay-face
   ((t ( :box nil
         :inverse-video t)))))

;; Briefly flash current line after a long movement.
(use-package pulsar
  :demand t
  :after evil
  :init
  (setq pulsar-face 'pulsar-generic)
  :custom-face
  (pulsar-generic
   ((t ( :background unspecified
         :inherit region))))
  :config
  (setq pulsar-functions
        '(evil-window-down
          evil-window-up
          evil-window-right
          evil-window-left
          evil-close-fold
          evil-open-fold
          evil-open-folds
          rex/hs-fold-imports-lang
          evil-avy-goto-char-timer
          other-window
          isearch-repeat-forward
          isearch-repeat-backward))
  (dolist (fkt pulsar-functions)
    (add-to-list 'pulsar-pulse-functions fkt))
  (pulsar-global-mode))

;; Show diffs in the fringe.
(use-package diff-hl
  :custom-face
  (diff-hl-insert
   ((t ( :background unspecified))))
  (diff-hl-change
   ((t ( :background unspecified))))
  (diff-hl-delete
   ((t ( :background unspecified))))
  :config
  (defun rex/diff-hl-settings-apply ()
    (interactive)
    (let* ((height (frame-char-height))
           (width 4)
           (ones (1- (expt 2 width)))
           (bits (make-vector height ones)))
      (define-fringe-bitmap 'rex/diff-hl-bitmap bits height width))
    (setq diff-hl-fringe-bmp-function (lambda (type pos) 'rex/diff-hl-bitmap)))
  :hook
  (prog-mode . diff-hl-mode)
  (diff-hl-mode . rex/diff-hl-settings-apply))


;; Face overrides
;; ----------------------------------------------------
(use-package emacs
  :custom-face
  (cursor
   ((t ( :background unspecified))))
  (whitespace-newline
   ((t ( :foreground unspecified
         :inherit font-lock-warning-face))))
  (whitespace-space
   ((t ( :foreground unspecified
         :inherit font-lock-warning-face))))
  (fringe
   ((t ( :background unspecified))))
  (variable-pitch
   ((t ( :family "monospace"))))
  (shadow
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face))))
  (help-key-binding
   ((t ( :foreground unspecified
         :background unspecified
         :weight bold
         :box unspecified))))
  (info-menu-star
   ((t ( :foreground unspecified))))
  (Info-quoted
   ((t ( :inherit nil
         :inherit font-lock-function-name-face)))))
