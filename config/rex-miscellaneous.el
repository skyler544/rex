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


(use-package pdf-tools
  :load-path "~/build/pdf-tools/lisp"
  :diminish Pdf-View-Midnight
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :config
  (pdf-tools-install t)
  (setq-default pdf-view-display-size 'fit-width))

(use-package image-roll
  :load-path "~/build/image-roll.el/"
  :init (require 'image-roll)
  :after pdf-view
  :hook (pdf-view-mode . pdf-view-roll-minor-mode)
  :config
  (setq image-roll-vertical-margin 0))
