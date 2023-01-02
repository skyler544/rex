;;; -*- lexical-binding: t -*-
;;
;; General settings
(load "rex-org-functions")
(use-package org
  :config
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-fontify-whole-heading-line t)
  (setq org-startup-indented t)
  (setq org-startup-folded nil)
  (setq org-hide-leading-stars t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-M-RET-may-split-line nil)
  (setq org-hide-emphasis-markers t)
  :hook
  (org-mode . auto-fill-mode))

;; Font settings
(use-package org
  :config
  (setq org-ellipsis " ⯆")
  ;; I prefer not to have lots of colors for different heading levels;
  ;; the indentation is enough and the colors seem noisy.
  (setq rex/org-levels
        '(org-level-1 org-level-2
          org-level-3 org-level-4
          org-level-5 org-level-6
          org-level-7 org-level-8))
  (dolist (face rex/org-levels)
    (set-face-attribute face nil :inherit nil :weight 'bold))
  (dolist (face '(org-block org-block-begin-line org-block-end-line))
    (set-face-attribute face nil :background nil)))

(use-package evil-org
  :config (defun rex/start-evil-org-mode ()
            (evil-org-mode))
  :after org
  :hook (org-mode . rex/start-evil-org-mode))

;; keybindings
(use-package org
  :general
  (:states 'normal
   :keymaps 'org-mode-map
            "RET" 'rex/org-dwim-at-point)
  (rex-leader
    "ma" 'org-agenda
    "ml" 'org-store-link)
  (rex-leader
    :keymaps 'org-mode-map
    "me" 'org-export-dispatch))

;; This package lets you pick how the leading symbol for each level of
;; heading should look in general; I use it to enforce a single bullet
;; style for plain org-mode headings. I prefer this because I already
;; prefer the headings to not have gaudy colors; why use gaudy unicode
;; symbols?
(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list '("●")))
