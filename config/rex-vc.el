;;; -*- lexical-binding: t -*-
;;
;; Version control utilities.
(use-package magit
  :config
  (setq magit-save-repository-buffers nil)
  :general
  (rex-leader
    "gg" 'magit-status))

;; Show previous versions of a file
(use-package git-timemachine
  :commands (git-timemachine))

;; Show diffs in the fringe.
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

;; Display git blame for the current line
(use-package why-this
  :general
  (rex-leader
    "tb" 'why-this-mode)
  :config
  (set-face-attribute 'why-this-face nil :foreground nil)
  (set-face-attribute 'why-this-face nil
                      :inherit 'font-lock-comment-face
                      :slant 'normal))

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
