;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Outliner, agenda, project manager
;; ----------------------------------------------------


;; General settings
;; ----------------------------------------------------
(use-package org
  :hook
  (org-mode . electric-pair-mode)
  (org-mode . auto-fill-mode)
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
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-ellipsis " ⯆")
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+")))

  (add-to-list 'org-file-apps '("\\.png\\'" . "feh %s"))
  (add-to-list 'org-export-backends 'md))


;; Agenda
;; ----------------------------------------------------
(use-package org
  :config
  (defun rex/agenda ()
    "Open the agenda with all todos."
    (interactive)
    (org-agenda 1 "n"))

  (defun rex/reload-agenda ()
    "Revert all org buffers and reload the agenda."
    (interactive)
    (org-revert-all-org-buffers)
    (org-agenda-redo))

  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits-only-for-today nil)
  (setq-default org-agenda-window-setup 'current-window)
  (setq org-agenda-span 10)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day "-3d")
  (setq org-agenda-files '("~/mega/org/todo.org" "~/mega/org/fh.org")))


;; TODOs / workflow settings
;; ----------------------------------------------------
(use-package org
  :config
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords '((sequence "TODO" "PROJ" "IDEA" "|" "DONE" "KILL") (sequence "NOTE" "HOLD")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:inherit (bold success org-todo)))
          ("PROJ" . (:inherit (bold warning org-todo)))
          ("NOTE" . (:inherit (bold warning org-todo)))
          ("IDEA" . (:inherit (bold font-lock-string-face org-todo)))
          ("DONE" . (:inherit (bold font-lock-comment-face org-todo)))
          ("HOLD" . (:inherit (bold font-lock-comment-face org-todo)))
          ("KILL" . (:inherit (bold error org-todo)))))

  ;; Stolen from doom, but trimmed down to only what I need.
  (defun rex/org-dwim-at-point (&optional arg)
    "Toggle the todo state of a headline or follow a link."
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
           (ignore-errors (org-toggle-checkbox (if (equal match "[ ]") '(16))))))))))


;; Font settings
;; ----------------------------------------------------
(use-package org
  :config
  (setq rex/org-levels
        '(org-level-1 org-level-2
          org-level-3 org-level-4
          org-level-5 org-level-6
          org-level-7 org-level-8))
  (dolist (face rex/org-levels)
    (set-face-attribute face nil
     :inherit nil
     :foreground 'unspecified
     :weight 'bold))

  :custom-face
  (org-agenda-date
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face
         :weight semi-bold))))
  (org-agenda-date-today
   ((t ( :foreground unspecified
         :inherit success
         :weight semi-bold))))
  (org-agenda-date-weekend
   ((t ( :foreground unspecified
         :inherit font-lock-keyword-face
         :weight semi-bold))))
  (org-date-selected
   ((t ( :foreground unspecified
         :inverse-video nil
         :inherit highlight))))
  (org-block
   ((t ( :background unspecified))))
  (org-block-begin-line
   ((t ( :background unspecified))))
  (org-block-end-line
   ((t ( :background unspecified))))
  (org-table
   ((t ( :foreground unspecified
         :inverse-video t))))
  (org-document-info-keyword
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face)))))


;; Keybindings
;; ----------------------------------------------------
(use-package org
  :general
  ( :keymaps 'org-read-date-minibuffer-local-map
    "C-h" '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
    "C-j" '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
    "C-k" '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
    "C-l" '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  ( :states 'normal
    :keymaps 'org-agenda-mode-map
    "R" 'rex/reload-agenda
    "q" 'org-agenda-quit
    "RET" 'org-agenda-goto
    "K" 'org-habit-toggle-display-in-agenda)
  ( :keymaps 'org-src-mode-map
    "C-c C-c" 'org-edit-src-exit)
  ( :states 'normal
    :keymaps 'org-mode-map
    "RET" 'rex/org-dwim-at-point)
  ( :states 'insert
    :keymaps 'org-mode-map
    "C-o" 'evil-org-open-below)
  (rex-leader
    "ma" 'rex/agenda
    "ml" 'org-store-link)
  (rex-leader
    :keymaps 'org-mode-map
    "mt" 'org-todo
    "md" 'org-deadline
    "mco" 'org-clock-out
    "mci" 'org-clock-in
    "me" 'org-export-dispatch))


;; Extensions
;; ----------------------------------------------------
(use-package org-contrib
  :after org
  :config
  (setq org-eldoc-breadcrumb-separator " -> "))

(use-package evil-org
  :after org
  :general
  (:states 'normal
   :keymaps 'org-agenda-mode-map
            "k" 'org-agenda-previous-line
            "j" 'org-agenda-next-line)
  (:keymaps 'evil-inner-text-objects-map
            "e" 'evil-org-inner-object
            "E" 'evil-org-inner-element
            "r" 'evil-org-inner-greater-element
            "R" 'evil-org-inner-subtree)
  :config (defun rex/start-evil-org-mode ()
            (evil-org-mode))
  :hook (org-mode . rex/start-evil-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list '("●")))

(use-package org-pdftools
  :after pdf-loader
  :commands org-pdftools-export
  :hook (org-mode . org-pdftools-setup-link))
