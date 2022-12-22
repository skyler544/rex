(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package corfu
  :init
  (global-corfu-mode))

(use-package dabbrev)
