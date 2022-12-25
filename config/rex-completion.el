(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
	("C-j" . corfu-next)
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("C-k" . corfu-previous)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :hook (eshell . (lambda ()
		    (setq-local corfu-auto nil)
		    (corfu-mode)))
  :init (global-corfu-mode))
