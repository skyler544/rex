;;; -*- lexical-binding: t -*-
;;
;; General programming utilities / settings
;; ****************************************
;; TODO: set up the integrated version
(use-package tree-sitter
  :diminish tree-sitter-mode)

(use-package tree-sitter-langs)

(use-package flymake :elpaca nil
  :init
  (setq-default flymake-no-changes-timeout 1)
  :config
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :general
  (rex-leader
    "tf" 'flymake-mode
    "pD" 'flymake-show-project-diagnostics))

(use-package emacs :elpaca nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package fancy-compilation
  :commands 'fancy-compilation-mode
  :config
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(use-package compile :elpaca nil
  :config
  (require 'ansi-color)
  (defun rex/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'rex/ansi-colorize-buffer)
  :custom-face
  (compilation-warning
   ((t ( :slant normal))))
  (compilation-error
   ((t ( :weight normal)))))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package imenu-list
  :config
  (setq imenu-list-mode-line-format nil)
  (setq imenu-list-position 'left)
  (setq imenu-list-size 0.2)
  :general
  (rex-leader
    "sl" 'imenu-list))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions  #'dumb-jump-xref-activate))

(use-package string-inflection)

;; Language server support
;; ****************************************
(use-package eglot :elpaca nil
  :defer t
  :commands (eglot)
  :custom-face
  (eglot-inlay-hint-face
   ((t ( :foreground unspecified
         :inherit font-lock-comment-face))))
  :general (rex-leader
    "cf" 'eglot-format-buffer
    "cd" 'eglot-find-declaration
    "cD" 'eglot-find-implementation
    "cr" 'eglot-rename
    "ca" 'eglot-code-actions)
  :config (add-to-list 'eglot-server-programs
                       '(php-mode . ("phpactor" "language-server")))
  :hook (eglot-managed-mode . (lambda ()
                                (setq eldoc-documentation-function
                                      'eldoc-documentation-compose-eagerly))))

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
            "M-h" 'eldoc-box-help-at-point))

;; Language config
;; ****************************************
(use-package cc-mode :elpaca nil
  :ensure nil
  :init
  (setq-default c-basic-offset 4)
  :hook
  (c-mode . eglot-ensure)
  (c-mode . tree-sitter-hl-mode))

(use-package java-mode :elpaca nil
  :ensure nil
  :hook
  (java-mode . eglot-ensure)
  (java-mode . tree-sitter-hl-mode)
  (java-mode . (lambda () (cl-defmethod eglot-execute-command
                            (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
                            "Eclipse JDT breaks spec and replies with edits as arguments."
                            (mapc #'eglot--apply-workspace-edit arguments)))))

(use-package php-mode
  :hook
  (php-mode . eglot-ensure)
  (php-mode . tree-sitter-hl-mode)
  :general
  (:keymaps '(normal php-mode)
            "gr" 'xref-find-references))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package yaml-mode
  :mode
  ("\\.yml.dist$" . yaml-mode)
  ("\\.yml$" . yaml-mode))

(use-package ess
  :hook (ess-r-mode . eglot-ensure)
  :mode ("\\.R$" . R-mode))

(use-package markdown-mode
  :custom-face
  (markdown-code-face
   ((t ( :background unspecified))))
  :mode ("\\.md$" . markdown-mode)
  :hook (markdown-mode . olivetti-mode))

(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.twig$" . web-mode))

(use-package nxml-mode :elpaca nil
  :ensure nil
  :mode ("\\.fxml$" . nxml-mode)
  :config
  (setq nxml-child-indent 4))

(use-package htmlize)

(use-package impatient-mode)

(use-package js
  :elpaca nil
  :ensure nil
  :hook
  (js-mode . tree-sitter-hl-mode)
  :mode ("\\.js$" . js-mode))

(use-package typescript-mode
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  :mode ("\\.ts$" . typescript-mode))

 (use-package emacs :elpaca nil
  :after eglot
  :init
  (define-derived-mode rex/vue-mode web-mode "rex/vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'eglot-server-programs '(rex/vue-mode "vls"))
  :mode ("\\.vue\\'" . rex/vue-mode)
  :hook
  (rex/vue-mode . eglot-ensure)
  (rex/vue-mode . (lambda () (electric-indent-local-mode -1))))

(use-package emacs :elpaca nil
  :hook
  (TeX-mode . display-line-numbers-mode)
  (TeX-mode . visual-line-mode)
  :config
  (setq tex-start-options "--shell-escape"))

(use-package sly
  :mode ("\\.cl\\'" . common-lisp-mode)
  :config (setq inferior-lisp-program "sbcl")
  :general
  (rex-leader
    :keymaps 'sly-mode-map
    "or" 'sly)
  (:keymaps 'sly-mode-map
            "M-h" 'sly-describe-symbol))
;; LaTeX
;; ****************************************
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package auctex-latexmk
  :hook (LaTeX-mode . (lambda () (setq TeX-command-default "LatexMk")))
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))
