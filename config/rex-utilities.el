;;; -*- lexical-binding: t -*-
;;

;; Show key hints onscreen; nice for getting to know a new mode,
;; somewhat distracting after a while. The delay can be toggled back
;; to 10000 if needed.
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 2)
  (setq which-key-add-column-padding 1)
  (setq which-key-side-window-slot -10)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; Better versions of some of the help functions. Add them to the C-h
;; map so that the entire prefix can be added to the general keybind
;; tree.
(use-package helpful
  :general
  ("C-h s" 'helpful-symbol)
  ("C-h f" 'helpful-callable)
  ("C-h k" 'helpful-key)
  ("C-h v" 'helpful-variable))

;; Colorize delimiters so that they indicate nesting depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; TODO Set this up; should make it possible to register commands and
;; select them via completion
(use-package run-command
  :general
  (rex-leader
    "R" 'run-command)
  :config
  (require 'run-command-recipes)
  (setq run-command-default-runner 'run-command-runner-eat)
  (setq run-command-recipes '(run-command-recipe-package-json run-command-recipe-make)))

;; TODO here is another very interesting idea
;; https://gitlab.com/howardabrams/emacs-piper

;; Terminal emulation
(use-package vterm
  :commands vterm)

(use-package eat
  :elpaca (:host codeberg :repo "akib/emacs-eat" :files (:defaults "./*"))
  :general
  (rex-leader
    "ot" 'eat
    "pt" 'eat-project)
  (:states 'normal
           :keymaps 'eat-semi-char-mode-map
           "RET" 'eat-self-input
           "p" 'eat-yank)
  :config
  (setq eat-term-scrollback-size nil)
  (add-to-list 'evil-insert-state-modes 'eat-mode))

;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions. TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :general
  (:states 'normal
           "f" 'avy-goto-char-timer))

;; Some nice editing/auxiliary functions.
(use-package crux
  :general
  (rex-leader
    "bK" 'crux-kill-other-buffers
    "fY" 'crux-kill-buffer-truename
    "fu" 'crux-sudo-edit
    "fD" 'crux-delete-file-and-buffer
    "fr" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:states '(normal visual)
           "?" 'iedit-mode)
  (:keymaps 'isearch-mode-map
            "C-;" 'iedit-mode-from-isearch))

(use-package goto-chg
  :general
  (:states 'normal
           "C-," 'goto-last-change))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 3))

;; Control temporary windows programmatically
(use-package popper
  :demand t
  :init
  (setq popper-mode-line
        (propertize " ▼ " 'face 'mode-line-emphasis))
  (setq popper-echo-mode t)
  (setq popper-window-height 0.3)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*help.*\\*" help-mode
          "\\*Calendar\\*"
          ("\\*Org Links\\*" . hide)
          "\\*Apropos\\*"
          "\\*Process List\\*"
          "\\*.*docker.*\\*"
          "\\*eldoc\\*" eldoc-mode
          "\\*sly" sly-mrepl-mode))
  :general
  (rex-leader
    "tc" 'popper-cycle
    "tl" 'popper-toggle-latest
    "tk" 'popper-kill-latest-popup
    "tp" 'popper-toggle-type)
  :config
  (popper-mode))

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

;; center buffer content in a window
(use-package olivetti
  :init
  (setq olivetti-body-width 0.8)
  :general
  (rex-leader
    "to" 'olivetti-mode))

(use-package expand-region
  :general
  (rex-leader
    "=" 'er/expand-region
    "-" 'er/contract-region))

;; allows you to increase font size globally
(use-package default-text-scale
  :config (default-text-scale-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; this shows long directory paths with a single file in them on one line
(use-package dired-collapse
  :elpaca (:host github :repo "Fuco1/dired-hacks"))

;; tree-like directory navigation; very slow, but it looks cool
(use-package dired-subtree
  :elpaca (:host github :repo "Fuco1/dired-hacks")
  :config
  (setq rex/dired-subtree-levels
        '(dired-subtree-depth-1-face dired-subtree-depth-2-face
          dired-subtree-depth-3-face dired-subtree-depth-4-face
          dired-subtree-depth-5-face dired-subtree-depth-6-face))
  (dolist (face rex/dired-subtree-levels)
    (set-face-attribute face nil :extend t))
  :general
  (:states 'normal
   :keymaps 'dired-mode-map
   "o" 'dired-subtree-cycle))

(use-package treemacs-all-the-icons)

(use-package treemacs
  :init
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  :general
  (rex-leader
    "tT" 'treemacs)
  :config
  (setq treemacs-wrap-around nil)
  :commands 'treemacs)

(use-package treemacs-evil)

(use-package hide-mode-line)

(use-package page-break-lines)

(use-package whitespace-mode :elpaca nil
  :ensure nil
  :general
  (rex-leader
    "tW" 'whitespace-mode))

(use-package rfc-mode
  :defer t
  :hook (rfc-mode . page-break-lines-mode))

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

(use-package restclient
  :mode ("\\.http" . restclient-mode))

(use-package shell-command-x
  :config
  (shell-command-x-mode 1))

;; built-in shell mode fixes
(use-package emacs :elpaca nil
  :custom-face
  (ansi-color-bright-black
   ((t ( :background unspecified
         :foreground unspecified))))
  :config
  (add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1)
  :general
  (:states 'normal
           "M-J" 'drag-stuff-down
           "M-K" 'drag-stuff-up))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package xref :elpaca nil
  :custom-face
  (xref-file-header
   ((t ( :foreground unspecified
         :inherit 'default
         :weight bold)))))

;; override this function so that xref output is a little more readable
;; TODO do this in a way that isn't so hacky
(defun xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...), where
GROUP is a string for decoration purposes and XREF is an
`xref-item' object."
  (require 'compile) ; For the compilation faces.
  (cl-loop for (group . xrefs) in xref-alist
           for max-line = (cl-loop for xref in xrefs
                                   maximize (xref-location-line
                                             (xref-item-location xref)))
           for line-format = (and max-line
                                  (format
                                   #("%%%dd: " 0 4 (face xref-line-number) 5 6 (face shadow))
                                   (1+ (floor (log max-line 10)))))
           with item-text-props = (list 'mouse-face 'highlight
                                        'keymap xref--button-map
                                        'help-echo
                                        (concat "mouse-2: display in another window, "
                                                "RET or mouse-1: follow reference"))
           with prev-group = nil
           with prev-line = nil
           do
           (xref--insert-propertized '(face xref-file-header xref-group t)
                                     group "\n")
           (dolist (xref xrefs)
             (pcase-let (((cl-struct xref-item summary location) xref))
               (let* ((line (xref-location-line location))
                      (prefix
                       (cond
                        ((not line) "  ")
                        ((and (equal line prev-line)
                              (equal prev-group group))
                         "")
                        (t (format line-format line)))))
                 ;; Render multiple matches on the same line, together.
                 (when (and (equal prev-group group)
                            (or (null line)
                                (not (equal prev-line line))))
                   (insert "\n"))
                 (xref--insert-propertized (nconc (list 'xref-item xref)
                                                  item-text-props)
                                           prefix summary)
                 (setq prev-line line
                       prev-group group))))
           (insert "\n\n"))
  (add-to-invisibility-spec '(ellipsis . t))
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (xref--apply-truncation)))
  (run-hooks 'xref-after-update-hook))

(use-package ispell :elpaca nil
  :ensure nil
  :config
  (setq ispell-program-name "aspell"))

(use-package dired-async
  :elpaca (:host github :repo "jwiegley/emacs-async")
  :diminish
  :custom-face
  (dired-async-message
   ((t ( :foreground unspecified
         :background unspecified
         :weight bold))))
  :config
  (dired-async-mode 1))
