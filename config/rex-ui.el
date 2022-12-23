(use-package emacs
  :init
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (setq default-frame-alist
	(append (list '(font . "Iosevka Custom Extended:size=20"))))

  (set-face-attribute 'variable-pitch nil
                    :family "Iosevka Custom Extended"
                    :slant 'oblique))

(use-package autothemer)

;; Manual installation of this theme to allow editing the modeline
;; without modifying the package inside the elpa folder.
;; https://github.com/emacsfodder/emacs-theme-creamsody
(use-package creamsody-theme
  :load-path "themes/emacs-theme-creamsody/"
  :init (require 'creamsody)
  :config
  (load-theme 'creamsody t)
  (creamsody-modeline))
