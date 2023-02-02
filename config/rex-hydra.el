;;; -*- lexical-binding: t -*-
;;
;; Makes creating 'keymap menus' simpler.
(use-package hydra
  :init
  (require 'windmove)
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))


  (defhydra +hydra/window-nav (:hint nil)
    "
Resize: _h_: left  _j_: down  _k_: up  _l_: right "
    ("h" hydra-move-splitter-left       )
    ("j" hydra-move-splitter-down       )
    ("k" hydra-move-splitter-up         )
    ("l" hydra-move-splitter-right      )
    ("q" nil                           ))

  (setq hydra--work-around-dedicated nil)
  (hydra-set-property '+hydra/window-nav :verbosity 0)

  :general
  (rex-leader
    "ww" '+hydra/window-nav/body))
