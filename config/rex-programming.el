;;; -*- lexical-binding: t -*-
;;
;; Add support for some programming tools such as language servers.
(use-package eglot
  :hook
  (c++-mode . eglot-ensure)
  (c-mode . eglot-ensure)
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
  :commands (eglot)
  :config (add-to-list 'eglot-server-programs
  '(php-mode . ("intelephense" "--stdio"))))

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


(use-package php-mode
  :mode ("\\.php$" . php-mode))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook
  (c-mode-common . tree-sitter-hl-mode)
  (java-mode . tree-sitter-hl-mode))

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
