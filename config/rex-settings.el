;;; -*- lexical-binding: t -*-
;;
;; General settings for built-in packages.
(use-package warnings
  :defer t
  :config
  ;; disable some automatic warnings
  (add-to-list 'warning-suppress-types '(iedit))
  (add-to-list 'warning-suppress-log-types '(iedit))
  (add-to-list 'warning-suppress-types '(eglot))
  (add-to-list 'warning-suppress-log-types '(eglot))
  (add-to-list 'warning-suppress-types '(use-package))
  (add-to-list 'warning-suppress-log-types '(use-package))
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-log-types '(comp))
  (add-to-list 'warning-suppress-types '(emacs))
  (add-to-list 'warning-suppress-log-types '(emacs)))

;; Save the 200 most recently visited files/directories for fast
;; access.
(use-package recentf
  :config
  (setq recentf-save-file (concat rex/cache-dir "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; line numbers
(use-package emacs
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))

;; general settings
(use-package emacs
  :config
  (put 'narrow-to-region 'disabled nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq-default tab-width 4)
  (setq-default fill-column 80)
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq Man-notify-method 'aggressive)
  (setq max-mini-window-height 8)
  (setq inhibit-compacting-font-caches t)
  (setq confirm-kill-processes nil)
  (setq byte-compile-warnings nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq word-wrap nil)
  (setq blink-cursor-mode nil)
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t)
  (menu-bar-mode -1)
  (column-number-mode))

;; mode line settings
(use-package emacs
  :config
  (setq mode-line-client nil)
  (setq-default
   mode-line-mule-info
   `("" (current-input-method
      (:propertize ("" current-input-method-title)
                   local-map ,mode-line-input-method-map
                   mouse-face mode-line-highlight))
     (:eval
      (propertize "-"
       'help-echo 'mode-line-mule-info-help-echo
       'mouse-face 'mode-line-highlight
       'local-map mode-line-coding-system-map))
     (:eval (mode-line-eol-desc)))))

;; minibuffer settings
(use-package emacs
  :config
  (setq enable-recursive-minibuffers t)
  (setq echo-keystrokes 0.02)
  (setq use-dialog-box nil)
  (setq completion-ignore-case t)
  (savehist-mode))

;; scrolling
(use-package emacs
  :config
  (setq scroll-margin 0)
  (setq scroll-conservatively 101)
  (setq scroll-preserve-screen-position t))

;; calendar
(use-package emacs
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-keyword-face)))

;; automatically make directories if necessary
(use-package emacs
  :config
  (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t))))))

;; built-in packages
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :general
  (rex-leader
    "ch" 'eldoc)
  :config
  (advice-add 'eldoc-doc-buffer :after (lambda () (other-window 1)))
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

;; Automatically closes pairs; works most of the time the right way
;; without needing to think about it.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Revert buffers on a timer
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

;; keep emacs directory cleaner
(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (concat rex/cache-dir "places"))
  (save-place-mode t))

(use-package eshell
  :ensure nil
  :config
  (setq eshell-directory-name (concat rex/cache-dir "eshell")))

(use-package project
  :ensure nil
  :config
  (setq project-list-file (concat rex/cache-dir "projects")))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (concat rex/cache-dir "bookmarks")))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-file (concat rex/cache-dir "minibuffer-history")))

(use-package transient
  :ensure nil
  :config
  (setq transient-history-file (concat rex/cache-dir "transient-history")))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-persistency-file-name (concat rex/cache-dir "tramp-persistency-file"))
  (setq tramp-auto-save-directory (concat rex/cache-dir "tramp-autosave")))

;; Dired settings
(use-package dired
  :ensure nil
  :config
  (defun rex/set-file-associations ()
    (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "zathura"))
    (add-to-list 'dired-guess-shell-alist-user '("\\.mp4\\'" "vlc")))
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t)
  :hook
  (dired-mode . rex/set-file-associations)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . (lambda () (load "dired-x"))))

(use-package proced
  :ensure nil
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 2))
