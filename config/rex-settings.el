(use-package warnings
  :defer t
  :config
  ;; disable some automatic warnings
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-log-types '(comp)))

(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package emacs
  :config
  ;; keep emacs directory cleaner
  (setq savehist-file (concat user-emacs-directory ".cache/minibuffer-history"))
  (setq transient-history-file (concat user-emacs-directory ".cache/transient-history"))
  (setq tramp-auto-save-directory (concat user-emacs-directory ".cache/tramp-autosave"))  

  ;; general settings
  (setq byte-compile-warnings nil)
  (setq make-backup-files nil)
  (setq word-wrap nil)
  (setq blink-cursor-mode nil)
  (column-number-mode)

  ;; minibuffer settings
  (setq enable-recursive-minibuffers t)
  (setq echo-keystrokes 0.02)
  (setq use-dialog-box nil)
  (setq completion-ignore-case t)
  (savehist-mode))
