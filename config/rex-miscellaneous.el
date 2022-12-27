;;; -*- lexical-binding: t -*-
;;
;; This file holds test code that may or may not work or come in handy someday.
(use-package emacs
  :config
  (defun rex/set-capf-nonexclusive (local-capf)
    (setq-local
     completion-at-point-functions
     (list (cape-capf-properties local-capf :exclusive 'no)))))
