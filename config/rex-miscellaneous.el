;;; -*- lexical-binding: t -*-
;;
;; This file holds test code that may or may not work or come in handy someday.
(use-package emacs
  :config
  (defun rex/set-capf-nonexclusive (local-capf)
    (setq-local
     completion-at-point-functions
     (list (cape-capf-properties local-capf :exclusive 'no)))))


(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO" "PROJ" "IDEA" "|" "DONE" "KILL")))
  (defface rex/todo
    '((t :inherit (bold font-lock-type-face org-todo))) "")
  (defface rex/proj
    '((t :inherit (bold warning org-todo))) "")
  (defface rex/idea
    '((t :inherit (bold font-lock-string-face org-todo))) "")
  (defface rex/done
    '((t :inherit (bold font-lock-comment-face org-todo))) "")
  (defface rex/kill
    '((t :inherit (bold error org-todo))) "")
  (setq org-todo-keyword-faces
        '(("TODO" . 'rex/todo)
          ("PROJ" . 'rex/proj)
          ("IDEA" . 'rex/idea)
          ("DONE" . 'rex/done)
          ("KILL" . 'rex/kill))))
