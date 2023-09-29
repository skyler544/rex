;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :init
  ;; most of this is stolen from doom
  (defvar rex/magit-open-windows-in-direction 'right
    "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")

  (defun rex/magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...
- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                '(display-buffer-same-window))

               ('(rex/magit--display-buffer-in-direction))))))

  (defun rex/magit--display-buffer-in-direction (buffer alist)
    "`display-buffer-alist' handler that opens BUFFER in a direction.
This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
    (let ((direction (or (alist-get 'direction alist)
                         rex/magit-open-windows-in-direction))
          (origin-window (selected-window)))
      (if-let (window (window-in-direction direction))
          (unless magit-display-buffer-noselect
            (select-window window))
        (if-let (window (and (not (one-window-p))
                             (window-in-direction
                              (pcase direction
                                (`right 'left)
                                (`left 'right)
                                ((or `up `above) 'down)
                                ((or `down `below) 'up)))))
            (unless magit-display-buffer-noselect
              (select-window window))
          (let ((window (split-window nil nil direction)))
            (when (and (not magit-display-buffer-noselect)
                       (memq direction '(right down below)))
              (select-window window))
            (display-buffer-record-window 'reuse window buffer)
            (set-window-buffer window buffer)
            (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
            (set-window-prev-buffers window nil))))
      (unless magit-display-buffer-noselect
        (switch-to-buffer buffer t t)
        (selected-window))))

  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-pre-refresh-hook nil)
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . hide))
  (setq magit-save-repository-buffers nil)
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  (setq magit-display-buffer-function #'rex/magit-display-buffer-fn)
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  (setq auto-revert-buffer-list-filter
        'magit-auto-revert-repository-buffer-p)
  :general
  (rex-leader
    "gg" 'magit-status))

;; Show previous versions of a file
(use-package git-timemachine
  :commands (git-timemachine))

;; Display git blame for the current line
(use-package why-this
  :diminish why-this-mode
  :general
  (rex-leader
    "tb" 'why-this-mode)
  :custom-face
  (why-this-face
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face
         :slant normal)))))

;; Disable support for obscure VCS
(use-package emacs :elpaca nil
  :config
  (setq-default vc-handled-backends '(Git)))

;; Change the vc-mode function to display the current project root
;; name instead of the vc backend. Since I have no reason to use any
;; other VCS than git, seeing the project name is more useful.
(use-package emacs :elpaca nil
  :config
  (defun rex/last-dir (str)
    (substring str (string-match "[^/]+/+$" str) -1))

  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     (concat " " (rex/last-dir (project-root (project-current)))) vc-mode)))
        (setq vc-mode noback)))))

;; Disable vc-mode when remotely editing
(use-package emacs :elpaca nil
  :config
  (defun rex/vc-off-remote ()
    "Disable vc-mode while editing remote files."
    (if (file-remote-p (buffer-file-name))
        (setq-local vc-handled-backends nil)))
  :hook (find-file . rex/vc-off-remote))

(use-package smerge-mode :elpaca nil
  :ensure nil
  :defer t
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))
