;;; -*- lexical-binding: t -*-
;;
;; General settings
(use-package org
  :config
  (setq org-link-frame-setup '((file . find-file)))
  (setq org-fontify-whole-heading-line t)
  (setq org-startup-indented t)
  (setq org-adapt-indentation t)
  (setq org-startup-folded nil)
  (setq org-hide-leading-stars t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-M-RET-may-split-line nil)
  (setq org-hide-emphasis-markers t)
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+")))
  :hook
  (org-mode . auto-fill-mode))

;; Agenda / workflow settings
(use-package org
  :config
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-agenda-span 10)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "-3d")
  (setq org-agenda-files '("~/mega/org/todo.org" "~/mega/org/fh.org" "~/mega/org/habits.org"))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords '((sequence "TODO" "PROJ" "IDEA" "|" "DONE" "KILL"))))

;; Font settings
(use-package org
  :config
  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold success org-todo)))
          ("PROJ" . (:inherit (bold warning org-todo)))
          ("IDEA" . (:inherit (bold font-lock-string-face org-todo)))
          ("DONE" . (:inherit (bold font-lock-comment-face org-todo)))
          ("KILL" . (:inherit (bold error org-todo)))))
  (set-face-attribute 'org-agenda-date nil
                      :foreground (face-foreground 'font-lock-comment-face)
                      :weight 'semi-bold)
  (set-face-attribute 'org-agenda-date-today nil
                      :foreground (face-foreground 'success)
                      :weight 'semi-bold)
  (set-face-attribute 'org-agenda-date-weekend nil
                      :foreground (face-foreground 'font-lock-keyword-face)
                      :weight 'semi-bold)
  (set-face-attribute 'org-date-selected nil
                      :foreground 'unspecified
                      :inverse-video 'unspecified
                      :inherit 'highlight)
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

(use-package org-contrib
  :config
  (setq org-eldoc-breadcrumb-separator "::"))

(use-package evil-org
  :general
  (:states '(normal visual)
   :keymaps 'org-agenda-mode-map
            "k" 'org-agenda-previous-line
            "j" 'org-agenda-next-line)
  :config (defun rex/start-evil-org-mode ()
            (evil-org-mode))
  :after org
  :hook (org-mode . rex/start-evil-org-mode))

(use-package org
  :config
  ;; A function that is mostly just stolen from Doom, but is trimmed down to only
  ;; do things that I need it to do.
  (defun rex/org-dwim-at-point (&optional arg)
    "Do what I mean at point. This will toggle the todo state of a
headline or follow a link."
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (pcase type
        ;; toggle todo state
        (`headline
         (cond ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     'todo
                   'done))))
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics))
        ;; follow a link
        (`link (org-open-at-point))
        ;; toggle a checkbox
        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))))))

;; keybindings
(use-package org
  :general
  (:keymaps 'org-read-date-minibuffer-local-map
            "C-h" '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
            "C-j" '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
            "C-k" '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
            "C-l" '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (:states 'normal
   :keymaps 'org-agenda-mode-map
            "RET" 'org-agenda-goto)
  (:keymaps 'org-src-mode-map
            "C-c C-c" 'org-edit-src-exit)
  (:states 'normal
   :keymaps 'org-mode-map
            "RET" 'rex/org-dwim-at-point)
  (:states 'insert
   :keymaps 'org-mode-map
            "C-o" 'evil-org-open-below)
  (rex-leader
    "ma" 'org-agenda
    "md" 'org-deadline
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
