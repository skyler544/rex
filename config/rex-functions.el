;;; -*- lexical-binding: t -*-
;;
;; This file defines any custom functions unrelated to specific
;; packages.
(use-package emacs
  :config
  (defun rex/scroll-other-window-down ()
    "Scroll the other window by a single line."
    (interactive)
    (scroll-other-window 1))
  (defun rex/scroll-other-window-up ()
    "Scroll the other window by a single line."
    (interactive)
    (scroll-other-window -1))
  (defun rex/split-and-follow-horizontally ()
    "Split window below and place point in the new window."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun rex/split-and-follow-vertically ()
    "Split window right and place point in the new window."
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun rex/shell-command-on-region-or-line ()
    "Run the command at point or in the selected region in the shell."
    (interactive)
    (async-shell-command (if (use-region-p)
                             (buffer-substring (region-beginning) (region-end))
                           (thing-at-point 'line t)))))
