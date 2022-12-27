;;; -*- lexical-binding: t -*-

(use-package emacs
  :config
  (setq tab-always-indent 'complete))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  (corfu-preselect 'prompt)
  (corfu-preselect-first nil)
  :general
  (:keymaps 'corfu-map
            "RET" nil
            "C-j" 'corfu-next
            [tab] 'corfu-next
            "C-k" 'corfu-previous
            [backtab] 'corfu-previous)
  :hook (eshell . (lambda ()
                    (setq-local corfu-auto nil)
                    (corfu-mode))))

