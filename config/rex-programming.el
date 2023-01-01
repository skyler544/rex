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
  :config
  (defun rex/open-jshell ()
    (interactive)
    (comint-run "jshell"))
    :general
  (rex-leader
    :keymaps 'java-mode-map
    "cR" 'eglot-java-run-main
    "cT" 'eglot-java-run-test
    "or" 'rex/open-jshell)
  :hook
  (java-mode . eglot-java-mode))

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))

(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook (java-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package flymake
  :ensure nil
  :config
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(use-package compile
  :ensure nil
  :config
  (set-face-attribute 'compilation-warning nil :slant 'normal)
  (set-face-attribute 'compilation-error nil :weight 'normal))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package flymake
  :ensure nil
  :general
  (rex-leader
    "tf" 'flymake-mode))
