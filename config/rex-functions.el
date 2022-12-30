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
    (scroll-other-window -1))
  (defun rex/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun rex/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :general
  (rex-leader
    "ws" 'rex/split-and-follow-horizontally
    "wv" 'rex/split-and-follow-vertically))
