;;; -*- lexical-binding: t -*-
;;
;; General settings for built-in packages.

;; general settings
(use-package emacs :elpaca nil
  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq-default tab-width 4)
  (setq-default fill-column 80)
  (setq-default sentence-end-double-space nil)
  (setq auto-hscroll-mode 'current-line)
  (setq inhibit-startup-screen t)
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq Man-notify-method 'aggressive)
  (setq max-mini-window-height 8)
  (setq inhibit-compacting-font-caches t)
  (setq confirm-kill-processes nil)
  (setq byte-compile-warnings nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq word-wrap t)
  (setq blink-cursor-mode nil)
  (setq redisplay-skip-fontification-on-input t)
  (menu-bar-mode -1)
  (column-number-mode)
  (subword-mode))

;; mode line settings
(use-package emacs :elpaca nil
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
(use-package emacs :elpaca nil
  :config
  (setq enable-recursive-minibuffers t)
  (setq echo-keystrokes 0.02)
  (setq use-dialog-box nil)
  (setq completion-ignore-case t)
  (savehist-mode))

;; Save the 200 most recently visited files/directories
(use-package emacs :elpaca nil
  :config
  (setq recentf-save-file (concat rex/cache-dir "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (setq ring-bell-function 'ignore)
  (defun emacs-session-save ())
  (recentf-mode 1)
  (add-to-list 'recentf-exclude '("\\/sudo:root@localhost.*"))
  (add-to-list 'recentf-exclude (recentf-expand-file-name "~/.emacs.d/.cache/bookmarks"))
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; line numbers
(use-package emacs :elpaca nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))

;; scrolling
(use-package emacs :elpaca nil
  :config
  (setq fast-but-imprecise-scrolling t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 101)
  (setq scroll-preserve-screen-position t)
  (pixel-scroll-precision-mode))

;; calendar
(use-package emacs :elpaca nil
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
(use-package emacs :elpaca nil
  :config
  (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t))))))

;; built-in packages
(use-package eldoc :elpaca nil
  :diminish eldoc-mode
  :general
  (rex-leader
    "ch" 'eldoc)
  :config
  (advice-add 'eldoc-doc-buffer :after
              (lambda (&optional interactive)
                (other-window -1)))
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package abbrev :elpaca nil
  :ensure nil
  :diminish abbrev-mode)

;; Automatically closes pairs; works most of the time the right way
;; without needing to think about it.
(use-package elec-pair :elpaca nil
  :hook (prog-mode . electric-pair-mode))

;; Revert buffers on a timer
(use-package autorevert :elpaca nil
  :diminish auto-revert-mode)

;; keep emacs directory cleaner
(use-package saveplace :elpaca nil
  :config
  (setq save-place-file (concat rex/cache-dir "places"))
  (save-place-mode t))

(use-package eshell :elpaca nil
  :defer t
  :config
  (setq eshell-directory-name (concat rex/cache-dir "eshell")))

(use-package project :elpaca nil
  :config
  (setq project-list-file (concat rex/cache-dir "projects")))

(use-package bookmark :elpaca nil
  :config
  (setq bookmark-default-file (concat rex/cache-dir "bookmarks")))

(use-package savehist :elpaca nil
  :config
  (setq savehist-file (concat rex/cache-dir "minibuffer-history")))

(use-package transient :elpaca nil
  :config
  (setq transient-history-file (concat rex/cache-dir "transient-history")))

(use-package tramp :elpaca nil
  :defer t
  :config
  (setq tramp-persistency-file-name (concat rex/cache-dir "tramp-persistency-file"))
  (setq tramp-auto-save-directory (concat rex/cache-dir "tramp-autosave")))

(use-package emacs :elpaca nil
  :config
  (setq auto-save-list-file-prefix (concat rex/cache-dir "auto-save-list")))

;; Dired settings
(use-package dired :elpaca nil
  :ensure nil
  :init
  (load "dired-x")
  :config
  (defun rex/set-file-associations ()
    (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "zathura"))
    (add-to-list 'dired-guess-shell-alist-user '("\\.mp4\\'" "vlc"))
    (add-to-list 'dired-guess-shell-alist-user '("\\.mkv\\'" "vlc")))
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-alh --group-directories-first --no-group")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . rex/set-file-associations)
  (dired-mode . dired-hide-details-mode))

(use-package proced :elpaca nil
  :defer t
  :custom-face
  (proced-executable
   ((t ( :foreground unspecified
         :inherit font-lock-escape-face))))
  (proced-cpu
   ((t ( :foreground unspecified
         :inherit font-lock-builtin-face))))
  (proced-mem
   ((t ( :foreground unspecified
         :inherit font-lock-builtin-face))))
  (proced-pid
   ((t ( :foreground unspecified
         :inherit font-lock-string-face))))
  (proced-emacs-pid
   ((t ( :foreground unspecified
         :weight bold
         :inherit font-lock-warning-face))))
  (proced-time-colon
   ((t ( :foreground unspecified
         :inherit font-lock-warning-face))))
  :init
  (setq-default proced-auto-update-flag t)
  :config
  (setq proced-tree-flag t)
  (setq proced-enable-color-flag t)
  (setq proced-auto-update-interval 2))
