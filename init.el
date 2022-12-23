(add-to-list 'load-path (concat user-emacs-directory "config/"))

;; do not add custom settings to the end of init.el
(setq custom-file "/dev/null")

(load "rex-bootstrap")
(load "rex-settings")
(load "rex-ui")
(load "rex-help")
(load "rex-movec")
(load "rex-completion")
(load "rex-keybindings")
(load "rex-git")
