(use-package magit
  :bind
  (:map magit-mode-map
	("x" . magit-delete-thing))
  (:map magit-status-mode-map
	("k" . previous-line)
	("j" . next-line)))

(use-package git-timemachine
  :commands (git-timemachine))
