(use-package emacs
  :init
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  :config
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/emacs-theme-creamsody/"))
  (load-theme 'creamsody t)
  (creamsody-modeline))

