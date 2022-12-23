(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer))

(use-package crux
  :bind (("S-<return>" . crux-smart-open-line)
	 ("C-S-<return>" . crux-smart-open-line-above)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("C-j" . crux-top-join-line)
	 ("C-S-k" . crux-kill-whole-line)
	 ("C-k" . kill-line)
	 ("C-c d" . crux-duplicate-current-line-or-region)))
