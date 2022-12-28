;;; -*- lexical-binding: t -*-
;;
;; This file defines any custom functions unrelated to specific
;; packages.
(use-package emacs
  :config
  (defun rex/scroll-other-window-down ()
    (interactive)
      (scroll-other-window 1))
  (defun rex/scroll-other-window-up ()
    (interactive)
      (scroll-other-window -1)))
