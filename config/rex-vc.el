(use-package magit
  :bind (:map magit-mode-map
	      ("x" . magit-delete-thing)
	      ("k" . magit-reset-quickly)))

(use-package git-timemachine
  :commands (git-timemachine))
