(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer))

(use-package crux
  :bind (("S-<return>" . crux-smart-open-line)
	 ("C-S-<return>" . crux-smart-open-line-above)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("C-S-k" . crux-kill-whole-line)
	 ("C-c d" . crux-duplicate-current-line-or-region)))
