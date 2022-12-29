;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :general
  (rex-leader
    "gg" 'magit-status))

;; Show previous versions of a file
(use-package git-timemachine
  :commands (git-timemachine))

;; Show diffs in the fringe.
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

;; Display git blame for the current line
(use-package why-this
  :general
  (rex-leader
    "tw" 'why-this-mode))
