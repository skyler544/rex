(use-package eglot
  :defer t
  :config
  ;; override this function to make the modeline less noisy
  (defun eglot--mode-line-format ()
    "Compose the Eglot's mode-line."
    (let* ((server (eglot-current-server))
           ;; (nick (and server (eglot-project-nickname server)))
           (nick "")
           (pending (and server (hash-table-count
                                 (jsonrpc--request-continuations server))))
           (last-error (and server (jsonrpc-last-error server))))
      (append
       `(,(propertize
           eglot-menu-string
           'face 'eglot-mode-line
           'mouse-face 'mode-line-highlight
           'help-echo "Eglot: Emacs LSP client\nmouse-1: Display minor mode menu"
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] eglot-menu)
                     map)))
       (when nick
         `(":"
           ,(propertize
             nick
             'face 'eglot-mode-line
             'mouse-face 'mode-line-highlight
             'help-echo (format "Project '%s'\nmouse-1: LSP server control menu" nick)
             'keymap (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line down-mouse-1] eglot-server-menu)
                       map))
           ,@(when last-error
               `("/" ,(eglot--mode-line-props
                       "error" 'compilation-mode-line-fail
                       '((mouse-3 eglot-clear-status  "Clear this status"))
                       (format "An error occurred: %s\n" (plist-get last-error
                                                                    :message)))))
           ,@(when (cl-plusp pending)
               `("/" ,(eglot--mode-line-props
                       (format "%d" pending) 'warning
                       '((mouse-3 eglot-forget-pending-continuations
                                  "Forget pending continuations"))
                       "Number of outgoing, still unanswered LSP requests to the server\n")))))))))
