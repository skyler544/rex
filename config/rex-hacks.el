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
still unanswered LSP requests to the server\n"))))))))
