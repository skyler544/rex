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

(use-package c-mode
  :ensure nil
  :hook (c-mode . (lambda () (setq c-basic-offset 4))))

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
  :init
  (setq-default flymake-no-changes-timeout 1)
  :config
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :general
  (rex-leader
    "tf" 'flymake-mode))

(use-package compile
  :ensure nil
  :custom-face
  (compilation-warning ((t (:slant normal))))
  (compilation-error ((t (:weight normal)))))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package ess
  :hook (ess-r-mode . eglot-ensure)
  :mode ("\\.R$" . R-mode))

(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))

(use-package eldoc-box
  :defer t
  :config
  (defun rex/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun rex/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  :general
  (:keymaps 'eglot-mode-map
            "C-k" 'rex/eldoc-box-scroll-up
            "C-j" 'rex/eldoc-box-scroll-down
            "M-h" 'eldoc-box-eglot-help-at-point))

(use-package treemacs
  :defer t
  :commands 'treemacs)

(use-package emacs
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))
