;;; -*- lexical-binding: t -*-
;;
;; General programming utilities / settings
;; ****************************************
;; https://github.com/casouri/tree-sitter-module
(use-package emacs :elpaca nil
  :diminish tree-sitter-mode)

;; fallback to the package for languages that don't have a dedicated mode yet
(use-package tree-sitter
  :demand t
  :diminish tree-sitter-mode
  :config
  (setq treesit-font-lock-level 4))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package treesit-auto
  :config
  (add-to-list 'global-treesit-auto-modes '(not org-mode))
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package flymake :elpaca nil
  :init
  (setq-default flymake-no-changes-timeout 1)
  :config
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :general
  (rex-leader
    "cn" 'flymake-goto-next-error
    "cp" 'flymake-goto-prev-error
    "tf" 'flymake-mode
    "cD" 'flymake-show-project-diagnostics))

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
  :config
  (with-eval-after-load 'eglot (fset #'jsonrpc--log-event #'ignore))
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0)
  (setq rex/language-servers
        (list '(rex/vue-mode "vls")
              '(prisma-mode "prisma-language-server" "--stdio")
              '(tsx-ts-mode "typescript-language-server" "--stdio")
              '(rex/mdx-mode "mdx-language-server" "--stdio")
              '(php-mode "phpactor" "language-server")))
  (dolist (server rex/language-servers)
    (add-to-list 'eglot-server-programs server))
  :hook
  (php-mode . eglot-ensure)
  (prisma-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (rex/mdx-mode . eglot-ensure)
  (rex/vue-mode . eglot-ensure)
  (eglot-managed-mode
   . (lambda () (setq eldoc-documentation-function
                      'eldoc-documentation-compose-eagerly))))

(use-package breadcrumb
  :elpaca (:host github :repo "joaotavora/breadcrumb"))

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

(use-package emacs :elpaca nil
  :config
  (define-derived-mode rex/vue-mode web-mode "rex/vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (define-derived-mode rex/mdx-mode tsx-ts-mode "rex/mdx"
    "A major mode derived from tsx-ts-mode, for editing .mdx files with LSP support.")
  :mode
  ("\\.vue\\'" . rex/vue-mode)
  ("\\.mdx\\'" . rex/mdx-mode)
  ("\\.env.test$" . conf-mode)
  ("\\.env.local$" . conf-mode)
  ("\\.env.sample$" . conf-mode)
  ("\\.env$" . conf-mode)
  (rex/vue-mode . (lambda () (electric-indent-local-mode -1))))

(use-package php-mode
  :hook (php-mode . tree-sitter-hl-mode)
  :config
  (setq php-mode-template-compatibility nil)
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
  :hook (lisp-mode . (lambda () (setq-local fill-column 72)))
  :general
  (rex-leader
    :keymaps 'sly-mode-map
    "or" 'sly)
  (:keymaps 'sly-mode-map
            "M-h" 'sly-describe-symbol))

(use-package lispy
  :diminish
  ;; :config
  ;; (lispy-set-key-theme '(lispy c-digits))
  :hook
  (emacs-lisp-mode . lispy-mode)
  (sly-mrepl-mode . lispy-mode)
  (lisp-mode . lispy-mode))

(use-package lispyville
  :diminish
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '(operators
          text-objects
          commentary
          additional
          ;; additional-movement
          ))
  :general
  ( :states '(normal insert)
    :keymaps 'lispyville-mode-map
    "M-L" 'lispyville->
    "M-H" 'lispyville-<))

;; LaTeX
;; ****************************************
(use-package tex
  :elpaca (:host github :repo "emacs-straight/auctex"
                 :pre-build (("chmod" "775" "autogen.sh") ("./autogen.sh")))
  :config
  (add-to-list 'TeX-expand-list
               '("%(-PDF)"
                 (lambda ()
                   (if TeX-PDF-mode
                       (cond ((eq TeX-engine 'default) "-pdf")
                             ((eq TeX-engine 'xetex) "-pdfxe")
                             ((eq TeX-engine 'luatex) "-pdflua")) ""))))
  (add-to-list 'TeX-command-list
               '("LaTeXmk" "latexmk %(-PDF) -%(PDF)%(latex)='%`%l%(mode)%'' %(output-dir) %t"
                 TeX-run-format nil (latex-mode doctex-mode) :help "Run Latexmk"))
  (with-eval-after-load 'latex
    (setq LaTeX-clean-intermediate-suffixes
          (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.fls")))))
