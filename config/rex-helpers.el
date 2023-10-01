;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Packages that help the user somehow.
;; ----------------------------------------------------


;; Emacs and readability help
;; ----------------------------------------------------
;; Disambiguate buffer names
(use-package emacs
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator " | ")
  (setq uniquify-after-kill-buffer-p t))

;; Even better help buffers
(use-package helpful
  :general
  ("C-c C-h" 'helpful-at-point)
  ("C-h s" 'helpful-symbol)
  ("C-h f" 'helpful-callable)
  ("C-h k" 'helpful-key)
  ("C-h v" 'helpful-variable))

;; Which-key helps you explore keymaps
(use-package which-key
  :config (which-key-mode))

;; Emacs can't handle very long lines
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Rainbow delimiters makes bracket levels easier to tell apart
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Use semantic indentation for wrapped lines
(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

;; Group buffers by project in ibuffer
(use-package ibuffer-project
  :config
  (setq ibuffer-project-use-cache t)
  (defun rex/enable-ibuffer-project ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook
  (ibuffer . rex/enable-ibuffer-project))

;; Remove unintentional whitespace edits
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))


;; Movement
;; ----------------------------------------------------
;; Jump around, jump around
(use-package avy
  :config (setq avy-all-windows t)
  :general
  (:states 'normal
           "f" 'avy-goto-char-timer))

;; Switch and swap windows more conveniently
(use-package iwindow
  :config
  (setq iwindow-selection-keys
	    '(97 115 100 102 103 104 106 107 108))
  :general
  ("C-x o"  'iwindow-select)
  (rex-leader
    "wo"  'iwindow-select))


;; Git
;; ----------------------------------------------------
;; Magit makes using git easier in every possible sense
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-pre-refresh-hook nil)
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . hide))
  (setq magit-save-repository-buffers nil)
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  (setq magit-display-buffer-function #'display-buffer)
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  (setq auto-revert-buffer-list-filter
        'magit-auto-revert-repository-buffer-p)
  :general
  (rex-leader
    "gg" 'magit-status))

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
