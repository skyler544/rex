;;; -*- lexical-binding: t -*-
;;
;; Add support for code completion and snippets.
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

(use-package cape
  :init
  (setq rex/capfs
        '(cape-dabbrev
          cape-file))
  (defun rex/add-capfs ()
    (dolist (fkt rex/capfs)
    (add-to-list 'completion-at-point-functions fkt)))
  (defun rex/set-capf-nonexclusive (local-capf)
    (setq-local
     completion-at-point-functions
     (list (cape-capf-properties local-capf :exclusive 'no))))
  (rex/add-capfs)
  :general
  (rex-leader
    "cd" 'cape-dabbrev
    "cf" 'cape-file))

(use-package emacs
  :config
  :hook
  (emacs-lisp-mode . (lambda () (rex/set-capf-nonexclusive #'elisp-completion-at-point))))

(use-package tempel)
