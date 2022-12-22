;; https://stackoverflow.com/questions/31079204/emacs-package-install-script-in-init-file

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(setq rex-packages
      '(use-package))

(when (not package-archive-contents) (package-refresh-contents))

(dolist (pkg rex-packages)
  (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
    (package-install pkg)))

(use-package use-package
  :init
  (setq use-package-always-ensure t))
