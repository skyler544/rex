(use-package emacs
  :init
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(use-package autothemer)

(use-package creamsody-theme
  :load-path "themes/emacs-theme-creamsody/"
  :init (require 'creamsody)
  :config
  (load-theme 'creamsody t)
  (creamsody-modeline))
