;;; -*- lexical-binding: t -*-
;;
;; Add support for code completion and snippets.
(use-package emacs
  :config
  (setq tab-always-indent 'complete))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-max-width 80)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  (corfu-preselect 'prompt)
  (corfu-preselect-first nil)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :general
  (:keymaps 'corfu-map
            "M-SPC" 'corfu-move-to-minibuffer
            "RET" nil
            "C-j" 'corfu-next
            [tab] 'corfu-next
            "C-k" 'corfu-previous
            [backtab] 'corfu-previous)
  :hook (eshell . (lambda ()
                    (setq-local corfu-auto nil)
                    (corfu-mode))))

(use-package cape
  :init
  (setq rex/capfs
        '(cape-dabbrev
          cape-file))
  (defun rex/add-capfs ()
    (dolist (fkt rex/capfs)
    (add-to-list 'completion-at-point-functions fkt)))
  (rex/add-capfs)
  :general
  (rex-leader
    "cd" 'cape-dabbrev
    "cf" 'cape-file))

(use-package tempel
  :general
  (rex-leader
    "ct" 'tempel-insert)
  (:keymaps 'insert
            "M-RET" 'tempel-expand)
  (:keymaps 'tempel-map
            "TAB" 'tempel-next
            "S-TAB" 'tempel-previous)
  :config
  (add-to-list 'rex/capfs 'tempel-expand)
  (rex/add-capfs))

(use-package tempel-collection
  :load-path "~/build/tempel-collection/"
  :ensure nil
  :init (require 'tempel-collection.el)
  :after tempel)
