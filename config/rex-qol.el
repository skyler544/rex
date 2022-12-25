;; Automatically closes pairs; works most of the time the right way
;; without needing to think about it.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Removes unintentional whitespace edits from lines that you've
;; touched while editing a file.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Provides many handy jump functions.  TODO: follow up on this
;; article and learn to use avy more in combination with embark:
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :bind ("C-;" . avy-goto-char-timer))

;; Some nice editing functions; not really necessary with evil.
(use-package crux
  :bind (("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c r" . crux-rename-file-and-buffer)))
