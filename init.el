;; Welcome to rex! This Emacs configuration is intended to be simple
;; to understand and extend. Emphasis is placed on the use of well-
;; documented, modular packages. Configuration is aided and organized
;; via use-package.
;;
;; Each use-package block should contain configuration for a single
;; package and should be entirely standalone. Disabling a use-package
;; block should not break the operation of any other package.
;;
;; Configuration for specific areas-of-concern should be done within a
;; standalone file inside the config/ directory. Such files may then
;; be explicitly loaded here. Disabling a file by removing or
;; commenting out the line that loads it should not break the rest of
;; the configuration. One exception is the bootstrap file; without it,
;; use-package will not be loaded and none of the rest of the
;; configuration will work.
(add-to-list 'load-path (concat user-emacs-directory "config/"))

;; Do not add custom settings to the end of init.el. This file is never
;; loaded; the intention is to have a place where the code generated
;; by custom can land, and if desired, may be added to the
;; configuration programmatically. This file may be deleted at any
;; time without adverse effects.
(setq custom-file (concat user-emacs-directory ".cache/custom.el"))

;; Strictly necessary for the operation of rex.
(load "rex-bootstrap")

;; Theme and font-face settings.
(load "rex-ui")

;; Help utilities.
(load "rex-help")

;; Marginalia Orderless Vertico Embark Consult
;; M.         O.        V.      E.     C.
(load "rex-movec")

;; Utilities related to code-completion.
(load "rex-completion")

;; Version-control helpers.
(load "rex-vc")

;; Packages considered "nice to have" or which otherwise improve the
;; user experience but do not fall into a more specific category.
(load "rex-qol")

;; Programming support.
(load "rex-programming")

;; Modal editing.
(load "rex-evil")

;; Global configuration unrelated to specific external packages. This
;; should be one of the last files loaded, otherwise packages may
;; override these settings.
(load "rex-settings")

;; Keybindings unrelated to specific external packages.
(load "rex-keybindings")
