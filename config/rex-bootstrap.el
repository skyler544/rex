;; This file initializes the package system and installs use-package.
;;
;; This file should not be modified frequently (if at all).

;; See this page for more information about the choices behind this
;; initialization method.
;; https://stackoverflow.com/questions/31079204/emacs-package-install-script-in-init-file

;; This setting must be made before (require 'use-package)
(setq use-package-enable-imenu-support t)

;; Add the melpa package archive and initialize the
;; package system.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Creating a list of packages in this way allows the code
;; to be more reusable, though this is not actually intended
;; to be used for installing anything other than use-package.
(setq rex-packages
      '(use-package))

(when (not package-archive-contents) (package-refresh-contents))

(dolist (pkg rex-packages)
  (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
    (package-install pkg)))

;; After this point, all further configuration should be made via use-package.
(use-package use-package
  :init
  (setq use-package-always-ensure t))

(use-package diminish
  :demand t)

(use-package general
  :demand t)
