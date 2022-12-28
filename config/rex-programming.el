;;; -*- lexical-binding: t -*-
;;
;; Add support for some programming tools such as language servers.
(use-package eglot
  :general
  (rex-leader
    "cf" 'eglot-format-buffer
    "cr" 'eglot-rename
    "ca" 'eglot-code-actions)
  :commands (eglot))

(use-package eglot-java
  :hook
  (java-mode . eglot-java-mode)
  (java-mode . (lambda () (setq tab-width 4))))

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))
