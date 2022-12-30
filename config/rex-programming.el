;;; -*- lexical-binding: t -*-
;;
;; Add support for some programming tools such as language servers.
(use-package eglot
  :config
  ;; override this function to make the modeline less noisy
  (defun eglot--mode-line-format ()
    "Compose the Eglot's mode-line."
    (pcase-let* ((server (eglot-current-server))
                 (pending (and server (hash-table-count
                                       (jsonrpc--request-continuations server))))
                 (`(,_id ,doing ,done-p ,_detail) (and server (eglot--spinner server)))
                 (last-error (and server (jsonrpc-last-error server))))
      (append
       `(,(propertize
           eglot-menu-string
           'face 'eglot-mode-line
           'mouse-face 'mode-line-highlight
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] eglot-menu)
                     map)))
       `(,@(when last-error
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "Clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                  :message)))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props doing
                                            'compilation-mode-line-run '())))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "Forget pending continuations"))
                     "Number of outgoing, \
still unanswered LSP requests to the server\n")))))))
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
