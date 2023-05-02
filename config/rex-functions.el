;;; -*- lexical-binding: t -*-
;;
;; This file defines any custom functions unrelated to specific
;; packages.
(use-package emacs :elpaca nil
  :config
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
                           (thing-at-point 'line t))))

  (defun rex/large-file-read-only ()
    "If a file is over a given size, make the buffer read only (and don't waste memory trying to use undo)"
    (when (> (buffer-size) (* 1024 1024))
      (setq buffer-read-only t)
      (buffer-disable-undo)))

  (defun rex/kill-relative-path ()
    "Kill the path to the currect file relative to the project root."
    (interactive)
    (kill-new (file-relative-name buffer-file-name (project-root (project-current t)))))

  :hook (find-file . rex/large-file-read-only))
