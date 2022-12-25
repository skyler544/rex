(use-package warnings
  :defer t
  :config
  ;; disable some automatic warnings
  (add-to-list 'warning-suppress-types '(iedit))
  (add-to-list 'warning-suppress-log-types '(iedit))
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-log-types '(comp))
  (add-to-list 'warning-suppress-types '(emacs))
  (add-to-list 'warning-suppress-log-types '(emacs)))

(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; keep emacs directory cleaner
(use-package emacs
  :config
  (setq savehist-file (concat user-emacs-directory ".cache/minibuffer-history"))
  (setq transient-history-file (concat user-emacs-directory ".cache/transient-history"))
  (setq tramp-auto-save-directory (concat user-emacs-directory ".cache/tramp-autosave")))

;; line numbers
(use-package emacs
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

;; general settings
(use-package emacs
  :config
  (setq-default indent-tabs-mode nil)
  (put 'narrow-to-region 'disabled nil)
  (setq confirm-kill-processes nil)
  (setq byte-compile-warnings nil)
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq word-wrap nil)
  (setq blink-cursor-mode nil)
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t)
  (column-number-mode))

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

;; builtin packages
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)
