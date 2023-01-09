;;; -*- lexical-binding: t -*-
;;
;; Welcome to rex! This Emacs configuration is intended to be simple
;; to understand and extend. Emphasis is placed on the use of well-
;; documented, modular packages. Configuration is aided and organized
;; via use-package. Key bindings are defined with general.el.
;;
;; Each use-package block should contain configuration for a single
;; package and should be entirely standalone. Disabling a use-package
;; block should not break the operation of any other package. This is
;; not always entirely feasible, but the effort should be made.
;;
;; Configuration for specific areas-of-concern should be done within a
;; standalone file inside the config/ directory. Such files may then
;; be explicitly loaded here. Disabling a file by removing or
;; commenting out the line that loads it should not break the rest of
;; the configuration. One exception is the bootstrap file; without it,
;; use-package will not be loaded and none of the rest of the
;; configuration will work.

(setq load-prefer-newer noninteractive)

;; https://github.com/karthink/.emacs.d/blob/cc34b599f4478441ab7eccb60b7516303a2b330d/early-init.el#L16
;; This snippet has something of the "cargo cult" about it, but it did shave a
;; few hundredths of a second off of the init time. Absolutely critical feature
;; here. -.-
(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun my/reset-file-handler-alist ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  (define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
    (advice-remove #'load-file #'load-file@silence)))

;; Don't warn about native comp stuff
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-deferred-compilation t)

;; Set up config and cache directories
(setq rex/config-dir (concat user-emacs-directory "config/"))
(setq rex/cache-dir (concat user-emacs-directory ".cache/"))
(add-to-list 'load-path rex/config-dir)

;; Do not add custom settings to the end of init.el. This file is never
;; loaded; the intention is to have a place where the code generated
;; by custom can land, and if desired, may be added to the
;; configuration programmatically. This file may be deleted at any
;; time without adverse effects.
(setq custom-file (concat rex/cache-dir "custom.el"))

;; Strictly necessary for the operation of rex.
(load "rex-bootstrap")

;; Custom elisp functions.
(load "rex-functions")

;; Set up the keymap tree.
(load "rex-keybindings")

;; Modal editing.
(load "rex-evil")

;; Hydra lets you create keymap menus
(load "rex-hydra")

;; Global configuration unrelated to specific external packages.
(load "rex-settings")

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

;; Utility packages
(load "rex-utilities")

;; Programming support.
(load "rex-programming")

;; Org mode, the swiss-army outline tool
(load "rex-org")

;; Hacky code
(load "rex-hacks")
