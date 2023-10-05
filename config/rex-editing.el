;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Settings and packages directly related to editing
;; ----------------------------------------------------


;; Time-savers
;; ----------------------------------------------------
(use-package emacs
  :hook
  ;; Automatically close parentheses/quotes/etc in programming modes.
  (prog-mode . electric-pair-mode)
  ;; Make scripts executable on save.
  (after-save . executable-make-buffer-file-executable-if-script-p))

;; Does what it says on the tin.
(use-package drag-stuff
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


;; Search and refactoring tools
;; ----------------------------------------------------
(use-package iedit
  :general
  (:states '(normal visual)
           "?" 'iedit-mode)
  (:keymaps 'isearch-mode-map
            "C-;" 'iedit-mode-from-isearch))

;; Switch between camelCase and snake_case etc.
(use-package string-inflection)

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


;; Editing merge conflicts
;; ----------------------------------------------------
(use-package emacs
  :hook (magit-diff-visit-file
         . (lambda ()
             (when smerge-mode
               (unpackaged/smerge-hydra/body)))))


;; Lisp editing
;; ----------------------------------------------------
;; TODO: tame these packages to be less disruptive without
;; completely hamstringing their usefulness
(use-package lispy
  :general
  ( :keymaps 'lispy-mode-map-special
    "b" nil)
  ( :keymaps 'lispy-mode-map-lispy
    "[" nil
    "]" nil)
  :hook
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '(operators
          text-objects
          commentary
          additional))
  :general
  ( :states '(normal insert)
    :keymaps 'lispyville-mode-map
    "M-L" 'lispyville->
    "M-H" 'lispyville-<))
