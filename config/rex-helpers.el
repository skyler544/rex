;;; -*- lexical-binding: t -*-
;;
;; Packages that help the user somehow.
;; ----------------------------------------------------
;; Helpful adds some even better help commands.
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

;; Rainbow delimiters makes bracket levels easier to tell apart
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Switch and swap windows more conveniently
(use-package iwindow
  :config
  (setq iwindow-selection-keys
	    '(97 115 100 102 103 104 106 107 108))
  :general
  ("C-x o"  'iwindow-select)
  (rex-leader
    "wo"  'iwindow-select))

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

;; Jump around, jump around
(use-package avy
  :config (setq avy-all-windows t)
  :general
  (:states 'normal
           "f" 'avy-goto-char-timer))

;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Make identically named buffers from different folders easier to distinguish.
(use-package emacs
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator " | ")
  (setq uniquify-after-kill-buffer-p t))

;; Use semantic indentation for wrapped lines
(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

;; Show buffers grouped by project in ibuffer
(use-package ibuffer-project
  :config
  (setq ibuffer-project-use-cache t)
  (defun rex/enable-ibuffer-project ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook
  (ibuffer . rex/enable-ibuffer-project))

;; Emacs can't handle very long lines; this helps with that.
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Use the built-in hideshow mode to hide some stuff programmatically.
(use-package hideshow
  :ensure nil
  :config
  (defvar rex/hs-fold-imports-alist
    '((php-mode . "^use ")
      (tsx-ts-mode . "^import {*[\s*\n*[:alnum:]*,* ]*\n*}*")
      (rex/mdx-mode . "^import {*[\s*\n*[:alnum:]*,* ]*\n*}*")
      (typescript-ts-mode . "^import ")))

  (defun rex/hs-fold-imports (pattern)
    (save-excursion
      (goto-char (point-min))
      (ignore-errors (re-search-forward pattern))
      (set-mark (point))
      (while (ignore-errors (re-search-forward pattern)))
      (ignore-errors (hs-hide-comment-region (region-beginning) (region-end)))
      (deactivate-mark t)))

  (defun rex/hs-fold-imports-lang ()
    "Hide the initial block of import statements in a buffer of `major-mode'."
    (interactive)
    (rex/hs-fold-imports
     (when (boundp 'rex/hs-fold-imports-alist)
       (alist-get major-mode rex/hs-fold-imports-alist))))

  :hook
  (prog-mode . hs-minor-mode)
  (php-mode . rex/hs-fold-imports-lang)
  (tsx-ts-mode . rex/hs-fold-imports-lang)
  (rex/mdx-mode . rex/hs-fold-imports-lang)
  :general
  (:keymaps 'prog-mode-map :states 'normal
            "zC" 'rex/hs-fold-imports-lang))
