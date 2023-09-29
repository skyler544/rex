;;; -*- lexical-binding: t -*-
;;
;; Settings directly related to editing
;; ----------------------------------------------------
(use-package emacs
  :hook
  ;; Automatically close parentheses/quotes/etc in programming modes.
  (prog-mode . electric-pair-mode)
  ;; Make scripts executable on save.
  (after-save . executable-make-buffer-file-executable-if-script-p))

;; Fast refactoring of text
(use-package iedit
  :general
  (:states '(normal visual)
           "?" 'iedit-mode)
  (:keymaps 'isearch-mode-map
            "C-;" 'iedit-mode-from-isearch))

;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "fY" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer))

;; Does what it says on the tin.
(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1)
  :general
  (:states 'normal
           "M-J" 'drag-stuff-down
           "M-K" 'drag-stuff-up))

;; Allows you to edit a region indirectly a-la org source blocks.
(use-package edit-indirect
  :general
  ( :states 'visual
    "C-c '" 'edit-indirect-region))

;; Better syntax highlighting for Emacs lisp.
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Switch between camelCase and snake_case etc.
(use-package string-inflection)

;; Highlight hex color strings (and some other kinds) in the buffer
(use-package rainbow-mode
  :commands 'rainbow-mode)


;; Search and refactoring tools
;; ----------------------------------------------------
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; Very slick text search
(use-package deadgrep
  :config
  (defun deadgrep-edit-mode ()
    (interactive)
    (wgrep-change-to-wgrep-mode))

  (defun rex/embark-become-deadgrep (&optional full)
    (interactive "P")
    (unless (minibufferp)
      (user-error "Not in a minibuffer"))
    (let* ((target (embark--display-string ; remove invisible portions
                    (if full
                        (minibuffer-contents)
                      (pcase-let ((`(,beg . ,end) (embark--boundaries)))
                        (string-remove-prefix
                         "#" (substring (minibuffer-contents) beg
                                        (+ end (embark--minibuffer-point)))))))))
      (embark--become-command #'deadgrep target)))

  :general
  (rex-leader
    "sD" 'deadgrep)
  (:keymaps 'embark-consult-async-search-map
            "D" 'deadgrep)
  (:keymaps 'minibuffer-mode-map
            "C-c C-d" 'rex/embark-become-deadgrep))

(use-package wgrep-deadgrep)

;; Whenever there is no better config, use this to try to find definitions.
(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions  #'dumb-jump-xref-activate))


;; Editing merge conflicts
;; ----------------------------------------------------
(use-package emacs
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))
