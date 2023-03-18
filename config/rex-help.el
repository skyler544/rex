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
