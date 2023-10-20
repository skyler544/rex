;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Basic editor setup
;; ----------------------------------------------------


;; Organization
;; ----------------------------------------------------
(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(use-package minions
  :config
  (setq minions-mode-line-lighter "--")
  (minions-mode))
(use-package delight)


;; Annoyance mitigation
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; Most of these warnings aren't relevant: "docstring wider than ..."
  (setq native-comp-async-report-warnings-errors nil)
  (setq warning-suppress-log-types '((comp) (bytecomp)))
  (setq warning-minimum-level :error)

  ;; Don't create backup/lockfiles. This is the 21st century; we have git.
  (setq create-lockfiles nil)
  (setq make-backup-files nil)

  ;; No beeping.
  (setq ring-bell-function 'ignore)

  ;; Avoid/streamline interactive questions.
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq load-prefer-newer noninteractive)
  (setq confirm-kill-processes nil))


;; General settings
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; Subjective editor defaults
  (setq-default sentence-end-double-space nil)
  (setq-default truncate-lines t)
  (setq-default fill-column 80)
  (setq-default tab-width 4)
  (setq-default word-wrap t)
  (setq-default indent-tabs-mode nil)

  ;; Enable some useful commands and features.
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (column-number-mode))


;; Tweaks
;; ----------------------------------------------------
(use-package emacs
  :config
  (setq-default vc-handled-backends '(Git))
  (setq inhibit-startup-screen t)

  ;; Don't restrict frame to the size of columns/rows.
  (setq frame-resize-pixelwise t)

  ;; Scrolling
  (setq-default auto-hscroll-mode 'current-line)
  (setq scroll-conservatively 101)
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 1)

  ;; Make `man' pop up in a real window.
  (setq Man-notify-method 'aggressive)

  ;; Make missing directories instead of uselessly failing.
  (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))

  ;; I don't need to see this stuff in the mode line all the time.
  (setq mode-line-client nil)
  (setq-default ; HACK: hide the coding system info
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

;; Change the vc-mode function to display the current project root
;; name instead of the vc backend.
(use-package emacs
  :config
  (defun rex/last-dir (str)
    (substring str (string-match "[^/]+/+$" str) -1))

  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     (concat " " (rex/last-dir (project-root (project-current)))) vc-mode)))
        (setq vc-mode noback)))))


;; Line numbers
;; ----------------------------------------------------
(use-package emacs
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))


;; Minibuffer
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; Keep the minibuffer small
  (setq max-mini-window-height 8)
  ;; Use minibuffer commands while using minibuffer commands
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)
  ;; Miscellaneous minibuffer settings.
  (setq echo-keystrokes 0.02)
  (setq use-dialog-box nil)
  (setq completion-ignore-case t)
  ;; eldoc
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-echo-area-use-multiline-p nil))


;; History
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; recentf
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 500)
  (setq recentf-auto-cleanup 60)
  (recentf-mode)
  (add-to-list 'recentf-exclude '("\\/sudo:root@localhost.*"))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; saveplace
  (save-place-mode t)
  ;; savehist
  (setq savehist-additional-variables
        '(file-name-history
          search-ring
          command-history
          regexp-search-ring))
  (setq savehist-ignored-variables '(magit-revision-history))
  (savehist-mode))


;; Calendar
;; ----------------------------------------------------
(use-package emacs
  ;; Always highlight today if it's visible
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-week-start-day 1)
  ;; Show week numbers
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-keyword-face)))


;; dired
;; ----------------------------------------------------
(use-package emacs
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

;; Don't block when copying/renaming.
(use-package dired-async
  :vc (:fetcher github :repo "jwiegley/emacs-async")
  :custom-face
  (dired-async-message
   ((t ( :foreground unspecified
         :background unspecified
         :weight bold))))
  :config
  (dired-async-mode 1))

;; Add some color to dired to improve readability
(use-package diredfl
  :hook (dired-mode . diredfl-mode))


;; proced
;; ----------------------------------------------------
(use-package proced
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


;; Fixes
;; ----------------------------------------------------
;; TODO: figure out why this is necessary
(use-package emacs
  :config
  ;; Add the info files from the cloned emacs source.
  (add-to-list 'Info-directory-list "~/build/emacs/info"))

;; Disable vc-mode when remotely editing
(use-package emacs
  :config
  (defun rex/vc-off-remote ()
    "Disable vc-mode while editing remote files."
    (if (file-remote-p (buffer-file-name))
        (setq-local vc-handled-backends nil)))
  :hook (find-file . rex/vc-off-remote))
