;; Automatically closes pairs; works most of the time the right way
;; without needing to think about it.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions.  TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :general
  ("C-;" 'avy-goto-char-timer))

;; Some nice editing functions; not really necessary with evil.
(use-package crux
  :general
  (rex-file-leader
    "r" 'crux-rename-file-and-buffer))

;; Fast refactoring of text
(use-package iedit
  :general
  (:states '(normal visual)
           "?" 'iedit-mode))

;; better pdf support
(use-package pdf-tools
  :diminish Pdf-View-Midnight
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width))

;; Briefly flash current line after a long movement.
(use-package pulsar
  :after evil
  :init
  (setq pulsar-face 'pulsar-green)
  :config
  (add-to-list 'pulsar-pulse-functions 'evil-window-down)
  (add-to-list 'pulsar-pulse-functions 'evil-window-up)
  (face-spec-reset-face 'pulsar-green)
  (set-face-attribute 'pulsar-green nil :inherit 'region)
  (pulsar-global-mode))
