;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Completion settings
;; ----------------------------------------------------


;; Built-in
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; Add tab support for code completion.
  (setq tab-always-indent 'complete))


;; Interactive popup
;; ----------------------------------------------------
;; Corfu displays a popup with current candidates from the active
;; completion-at-point-function. This setup is intended to be used
;; only with `TAB' and `S-TAB'; that is, it pops up and starts
;; completion and you press `TAB' and `S-TAB' to cycle the
;; candidates. `ESC' stops completion and brings you back to what you
;; had before. Typing anything else will quit completion.
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
  (corfu-auto-delay 0)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary t)

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  :general
  (:keymaps 'corfu-map
            "RET" nil
            "C-j" 'corfu-next
            [tab] 'corfu-next
            "C-k" 'corfu-previous
            [backtab] 'corfu-previous)

  :hook
  (evil-insert-state-exit . corfu-quit)
  (minibuffer-setup . corfu-enable-in-minibuffer)
  (eshell . (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

;; Cape provides capfs for better in-buffer completion, as well as ways to
;; combine / transform such functions.
(use-package cape
  :init
  (setq rex/capfs '(cape-dabbrev cape-file))
  (defun rex/add-capfs ()
    (dolist (fkt rex/capfs)
    (add-to-list 'completion-at-point-functions fkt)))
  (rex/add-capfs)
  :general
  (rex-leader
    "cd" 'cape-dabbrev
    "cf" 'cape-file))


;; Snippets
;; ----------------------------------------------------
;; Tempel provides a framework for defining / using snippets in plain elisp.
(use-package tempel
  :general
  (rex-leader
    "ct" 'tempel-insert)
  (:keymaps 'prog-mode-map
            "C-c e" 'tempel-expand)
  (:keymaps 'org-mode-map
            "C-c e" 'tempel-expand)
  (:keymaps 'tempel-map
            "TAB" 'tempel-next
            "S-TAB" 'tempel-previous))

;; Some predefined snippets
(use-package tempel-collection
  :vc (:fetcher github :repo "Crandel/tempel-collection")
  :init (require 'tempel-collection)
  :after tempel)
