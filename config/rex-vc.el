;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers nil)
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
   ((t (:foreground nil :inherit font-lock-comment-face :slant normal)))))

;; Disable support for obscure VCS
(use-package emacs
  :config
  (setq-default vc-handled-backends '(Git)))

;; Change the vc-mode function to display the current project root
;; name instead of the vc backend. Since I have no reason to use any
;; other VCS than git, seeing the project name is more useful.
(use-package emacs
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
(use-package emacs
  :config
  (defun rex/vc-off-remote ()
    "Disable vc-mode while editing remote files."
    (if (file-remote-p (buffer-file-name))
        (setq-local vc-handled-backends nil)))
  :hook (find-file . rex/vc-off-remote))
