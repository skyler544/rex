(use-package emacs
  :config
  ;; disable some automatic warnings
  (add-to-list 'warning-suppress-types '((comp)))
  (add-to-list 'warning-suppress-log-types '((comp)))

  ;; keep emacs directory cleaner
  (setq savehist-file (concat user-emacs-directory ".cache/minibuffer-history"))
  (setq transient-history-file (concat user-emacs-directory ".cache/transient-history"))
  (setq tramp-auto-save-directory (concat user-emacs-directory ".cache/tramp-autosave"))  
  
  ;; general settings
  (column-number-mode)
  (setq use-dialog-box nil))
