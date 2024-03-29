;;; -*- lexical-binding: t -*-
;;
;; Language specific configuration and tools
;; ----------------------------------------------------


;; Lisp
;; ----------------------------------------------------
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl")
  (setq sly-command-switch-to-existing-lisp 'always)
  (setq sly-description-autofocus t)
  (setq sly-mrepl-prevent-duplicate-history 'move)
  :hook
  (sly-mode . (lambda () (unless (sly-connected-p) (sly))))
  (lisp-mode . (lambda () (setq-local corfu-auto-prefix 4)))
  (lisp-mode . (lambda () (setq-local fill-column 72)))
  :general
  (rex-leader
    :keymaps 'sly-mode-map
    "or" 'sly)
  (:keymaps 'sly-mode-map
            "M-h" 'sly-describe-symbol))

;; Better syntax highlighting for Emacs lisp.
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))


;; C/C++
;; ----------------------------------------------------
(use-package emacs
  :config
  (setq-default c-basic-offset 4)
  :mode
  ("\\.c$" . c-ts-mode)
  ("\\.h$" . c-ts-mode)
  ("\\.cpp$" . c++-ts-mode)
  ("\\.hpp$" . c++-ts-mode)
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure))


;; Java
;; ----------------------------------------------------
(use-package emacs
  :mode ("\\.java$" . java-ts-mode)
  :hook
  (java-ts-mode . eglot-ensure)
  (java-ts-mode
   . (lambda () (cl-defmethod eglot-execute-command
                  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
                  "Eclipse JDT breaks spec and replies with edits as arguments."
                  (mapc #'eglot--apply-workspace-edit arguments)))))


;; JavaScript/Typescript
;; ----------------------------------------------------
(use-package emacs
  :after treesit
  :custom-face
  (typescript-ts-jsx-tag-face
   ((t ( :inherit font-lock-type-face))))
  :delight
  (typescript-ts-mode "TS")
  (tsx-ts-mode "TS[tsx]")
  (js-ts-mode "JS")
  :mode
  ("\\.js$" . js-ts-mode)
  ("\\.ts$" . typescript-ts-mode)
  ("\\.tsx$" . tsx-ts-mode))

(use-package prisma-mode
  :vc (:fetcher github :repo "pimeys/emacs-prisma-mode"))

(use-package emacs
  :config
  (define-derived-mode rex/mdx-mode tsx-ts-mode "rex/mdx"
    "A major mode derived from tsx-ts-mode, for editing .mdx files with LSP support.")
  :mode
  ("\\.mdx\\'" . rex/mdx-mode))

(use-package nvm
  :vc (:fetcher github :repo "rejeep/nvm.el")
  :init
  (setq rex/nvm-enabled nil)
  (defun rex/load-nvm ()
    "Start nvm."
    (interactive)
    (setq rex/nvm-enabled t)
    (async-shell-command "source ~/.local/bin/load-nvm"))
  (defun rex/nvm-use ()
    "Use the .nvmrc file."
    (interactive)
    (unless rex/nvm-enabled
      (rex/load-nvm))
    (nvm-use-for))
  :general
  (rex-leader
    "bv" 'rex/nvm-use))

(use-package add-node-modules-path
  :hook
  (tsx-ts-mode . add-node-modules-path)
  (typescript-ts-mode . add-node-modules-path))

(use-package prettier-js
  :hook
  (tsx-ts-mode . prettier-js-mode)
  (typescript-ts-mode . prettier-js-mode))


;; PHP
;; ----------------------------------------------------
(use-package php-mode
  :hook (php-mode . tree-sitter-hl-mode)
  :config
  (setq php-mode-template-compatibility nil)
  :general
  (:keymaps '(normal php-mode)
            "gr" 'xref-find-references))


;; Markdown
;; ----------------------------------------------------
(use-package markdown-mode
  :custom-face
  (markdown-code-face
   ((t ( :background unspecified))))
  :mode ("\\.md$" . markdown-mode)
  :hook (markdown-mode . olivetti-mode))


;; LaTeX
;; ----------------------------------------------------
(use-package tex
  :ensure auctex
  :hook
  (TeX-mode . display-line-numbers-mode)
  (TeX-mode . visual-line-mode)
  :config
  (setq TeX-parse-self t)
  (add-to-list 'TeX-expand-list
               '("%(-PDF)"
                 (lambda ()
                   (if TeX-PDF-mode
                       (cond ((eq TeX-engine 'default) "-pdf")
                             ((eq TeX-engine 'xetex) "-pdfxe")
                             ((eq TeX-engine 'luatex) "-pdflua")) ""))))
  (add-to-list 'TeX-command-list
               '("LaTeXmk" "latexmk -shell-escape %(-PDF) -%(PDF)%(latex)='%`%l%(mode)%'' %(output-dir) %t"
                 TeX-run-format nil (latex-mode doctex-mode) :help "Run Latexmk")))


;; Miscellaneous
;; ----------------------------------------------------
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package dockerfile-mode)

(use-package yaml-mode
  :mode
  ("\\.ya?ml$" . yaml-mode))

(use-package yaml-pro
  :hook
  (yaml-mode . yaml-pro-ts-mode))

(use-package emacs
  :mode
  ("\\.env$" . conf-mode)
  ("\\.env.test$" . conf-mode)
  ("\\.env.local$" . conf-mode)
  ("\\.env.sample$" . conf-mode))
