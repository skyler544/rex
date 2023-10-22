;;; -*- lexical-binding: t -*-
;;
;; Window management
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; ----------------------------------------------------
(use-package emacs
  :config

  ;; Winner Mode records changes to your window configuration and allows
  ;; you to undo/redo changes to that configuration.
  (winner-mode)

  ;; Reconsider this; misbehaving packages might abuse `switch-to-buffer' but
  ;; with this variable set to `t', invoking `switch-to-buffer' manually also
  ;; follows the rules set via `display-buffer-alist.'
  ;; (setq switch-to-buffer-obey-display-actions t)

  (defun rex/toggle-window-dedication ()
    "Toggles window dedication in the selected window."
    (interactive)
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window)))))

  (add-to-list 'display-buffer-alist
               '("\\*sly-mrepl"
                 (display-buffer-at-bottom)
                 (window-height . 12)))
  (add-to-list 'display-buffer-alist
               '("\\*Calendar*"
                 (display-buffer-at-bottom)))
  (add-to-list 'display-buffer-alist
               '("^\*.*\\[.*\\]\*"
                 (display-buffer-at-bottom)
                 (window-height . 12)))
  (add-to-list 'display-buffer-alist
               '("\\*shell:"
                 (display-buffer-at-bottom)
                 (window-height . 12)))
  (add-to-list 'display-buffer-alist
               '("\\magit:"
                 (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Man"
                 (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Help"
                 (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*helpful"
                 (display-buffer-same-window))))
