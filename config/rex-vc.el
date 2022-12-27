;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :general
  (rex-leader
    "gg" 'magit-status))

(use-package git-timemachine
  :commands (git-timemachine))
