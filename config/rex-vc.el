(use-package magit
  :defer t
  :bind (:map magit-mode-map
	      ("x" . magit-delete-thing)
	      ("k" . magit-reset-quickly)))

(use-package git-timemachine
  :defer t)
