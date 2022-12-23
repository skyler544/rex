;; Welcome to rex! This Emacs configuration is intended to be simple
;; to understand and extend. Emphasis is placed on the use of well-
;; documented packages. Configuration is aided and organized via
;; use-package.
;;
;; Each use-package block should contain configuration for a single
;; package and should be entirely standalone. Disabling a use-package
;; block should not break the operation of any other package.
;;
;; Configuration for specific areas-of-concern should be done
;; within a standalone file inside the config/ directory. Such
;; files may then be explicitly loaded here. Disabling a file
;; by removing or commenting out the line that loads it should
;; not break the rest of the configuration. One exception is the
;; bootstrap file; without it, use-package will not be loaded
;; and none of the rest of the configuration will work.
(add-to-list 'load-path (concat user-emacs-directory "config/"))


;; do not add custom settings to the end of init.el
(setq custom-file ".cache/custom.el")

;; strictly necessary for the operation of rex.
(load "rex-bootstrap")

;; configuration unrelated to specific external packages
(load "rex-settings")

;; keybindings unrelated to any specific external packages
(load "rex-keybindings")
;; theme and font-face settings
(load "rex-ui")

;; help utilities
(load "rex-help")

;; Marginalia Orderless Vertico Embark Consult
;; M.         O.        V.      E.     C.
(load "rex-movec")

;; utilities related to code-completion
(load "rex-completion")

;; Magit
(load "rex-git")

;; packages considered "nice to have" or which otherwise
;; improve the user experience but do not fall into a
;; more specific category
(load "rex-qol")
