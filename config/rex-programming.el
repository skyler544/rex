;;; -*- lexical-binding: t -*-
;;
;; Add support for some programming tools such as language servers.
(use-package eglot
  :hook
  (eglot-managed-mode . (lambda ()
                          (setq eldoc-documentation-function
                                'eldoc-documentation-compose-eagerly)))
  :general
  (rex-leader
    "ch" 'eldoc
    "cf" 'eglot-format-buffer
    "cd" 'eglot-find-declaration
    "cD" 'eglot-find-implementation
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

(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook (java-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)
