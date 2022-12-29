;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :general
  (rex-leader
    "gg" 'magit-status))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package why-this
  :general
  (rex-leader
    "tw" 'why-this-mode))
