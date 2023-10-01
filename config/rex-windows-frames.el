;;; -*- lexical-binding: t -*-
;;
;; Window and frame management
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; Pixel resize, not column/line resize.
  (setq frame-resize-pixelwise t)

  ;; Winner Mode records changes to your window configuration and allows
  ;; you to undo/redo changes to that configuration.
  (winner-mode)

  (setq switch-to-buffer-obey-display-actions t)

  (defun rex/toggle-window-dedication ()
    "Toggles window dedication in the selected window."
    (interactive)
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window)))))

  (add-to-list 'display-buffer-alist
               '("\\*sly-mrepl"
                 (display-buffer-at-bottom)
                 (dedicated . t)
                 (window-height . 10)))
  (add-to-list 'display-buffer-alist
               '("\\*shell:"
                 (display-buffer-same-window)))
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

;; Provides `C-M-+', `C-M--', `C-M-0' keybindings for
;; increasing/decreasing/resetting font size for the
;; whole frame at once.
(use-package default-text-scale
  :config (default-text-scale-mode))
