;;; -*- lexical-binding: t -*-
;;
;; Support tools for programming languages.
;; ----------------------------------------------------


;; Tree-Sitter
;; ----------------------------------------------------
;; Abstract Syntax Tree based font-locking.
(use-package treesit-auto
  :config
  (add-to-list 'global-treesit-auto-modes '(not org-mode))
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

;; Fall back to external package for languages that don't have a dedicated mode yet
(use-package tree-sitter
  :demand t
  :config
  (setq treesit-font-lock-level 4))

(use-package tree-sitter-langs
  :after tree-sitter)


;; Compilation
;; ----------------------------------------------------
(use-package emacs
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

(use-package fancy-compilation
  :commands 'fancy-compilation-mode
  :config
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))


;; Symbol mapping and documentation lookup
;; ----------------------------------------------------
(use-package imenu-list
  :config
  (setq imenu-list-mode-line-format nil)
  (setq imenu-list-position 'left)
  (setq imenu-list-size 0.2)
  :general
  (rex-leader
    "sl" 'imenu-list))

(use-package breadcrumb
  :vc (:fetcher github :repo "joaotavora/breadcrumb"))

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


;; Language server protocol
;; ----------------------------------------------------
(use-package emacs
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
  (with-eval-after-load 'eglot
    (setq rex/language-servers
          (list '(prisma-mode "prisma-language-server" "--stdio")
                '(tsx-ts-mode "typescript-language-server" "--stdio")
                '(rex/mdx-mode "mdx-language-server" "--stdio")
                '(php-mode "phpactor" "language-server")))
    (dolist (server rex/language-servers)
      (add-to-list 'eglot-server-programs server)))
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
