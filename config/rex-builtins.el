;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Config and extensions for builtin packages
;; ----------------------------------------------------


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

;; tree-like directory navigation; very slow, but it looks cool
(use-package dired-subtree
  :vc (:fetcher github :repo "Fuco1/dired-hacks")
  :config
  (setq rex/dired-subtree-levels
        '(dired-subtree-depth-1-face dired-subtree-depth-2-face
          dired-subtree-depth-3-face dired-subtree-depth-4-face
          dired-subtree-depth-5-face dired-subtree-depth-6-face))
  (dolist (face rex/dired-subtree-levels)
    (set-face-attribute face nil :extend t))
  :general
  ( :states 'normal
    :keymaps 'dired-mode-map
    "o" 'dired-subtree-cycle))


;; tramp
;; ----------------------------------------------------
(use-package emacs
  :after 'tramp
  :config
  (setq tramp-default-method 'ssh)

  (setq tramp-verbose 1)

  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))


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
