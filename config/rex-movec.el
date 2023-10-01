;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Package ecosystem for the minibuffer and completion
;; ----------------------------------------------------


;; Vertico provides a vertical view of minibuffer candidates.
;; ----------------------------------------------------
(use-package vertico
  :init
  (vertico-mode)
  :custom-face
  (vertico-group-title
   ((t ( :inherit font-lock-comment-face))))
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
  :config
  (vertico-multiform-mode)

  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun rex/vertico-highlight-file-base-name (file)
    "If FILE is a relative path with directory components, highlight only the base filename."
    (if (and (string-match "/" file) (not (string-suffix-p "/" file)))
        (progn (add-face-text-property
                (length (file-name-directory file)) (length file)
                'font-lock-escape-face nil file)
               file)
      file))

  (add-to-list 'vertico-multiform-categories
               '(project-file (+vertico-transform-functions . rex/vertico-highlight-file-base-name)))

  (setq vertico-cycle t)
  (setq vertico-count 12)
  (setq vertico-resize nil))

;; This extends vertico with keybindings for navigating directories.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :general
  (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char
            "M-DEL" 'vertico-directory-delete-word)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Orderless provides better matching in the minibuffer.
;; ----------------------------------------------------
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file) (styles partial-completion))))


;; Marginalia adds context to minibuffer matches.
;; ----------------------------------------------------
(use-package marginalia
  :init
  (marginalia-mode))


;; Embark provides actions for candidates on-the-fly.
;; ----------------------------------------------------
(use-package embark
  :general
  ("C-;" 'embark-act)
  ("C-'" 'embark-act-all)
  ("C-h B" 'embark-bindings)
  (rex-leader
    "a" 'embark-act)
  (:keymaps 'minibuffer-mode-map
            "C-c C-e" 'embark-export)
  :config
  (setq embark-indicators '(embark-minimal-indicator))
  (setq embark-prompter 'embark-completing-read-prompter))


;; Consult provides many enhanced commands for streamlining your workflow.
;; ----------------------------------------------------
(use-package consult
  :config

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (consult-customize consult-buffer :preview-key nil)

  (defun rex/consult-ripgrep-in-a-directory ()
    "Query the user for a directory to grep in."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'consult-ripgrep)))

  (defun rex/consult-ripgrep-in-current-directory ()
    "Run consult-grep in the current directory."
    (interactive)
    (consult-ripgrep default-directory))

  (defun rex/consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (action cand)
                    (if (or (eq action 'return)
                            (let ((buffer (get-buffer cand)))
                              (and buffer
                                   (not (file-remote-p (buffer-local-value 'default-directory buffer))))))
                        cand
                      nil))))
      (lambda (action cand)
        (funcall orig-state action (funcall filter action cand)))))

  (setq consult--source-buffer
        (plist-put consult--source-buffer :state #'rex/consult-buffer-state-no-tramp))

  :general
  ("C-x b" 'consult-buffer)
  (:states '(normal visual)
           "P" 'consult-yank-from-kill-ring
           "," 'consult-line)
  (:keymaps 'vertico-map
            "M-s" 'consult-history)
  (rex-leader
    ":" 'consult-complex-command
    "," 'consult-buffer
    ;; buffers
    "bb" 'consult-bookmark
    ;; code
    "ce" 'consult-flymake
    ;; files
    "fF" 'consult-find
    ;; goto
    "gl" 'consult-goto-line
    "gm" 'consult-mark
    "gk" 'consult-global-mark
    ;; project
    "pb" 'consult-project-buffer
    ;; search
    "sd" 'rex/consult-ripgrep-in-current-directory
    "sD" 'rex/consult-ripgrep-in-a-directory
    "si" 'consult-imenu
    "sI" 'consult-imenu-multi
    "sl" 'consult-line
    "se" 'consult-isearch-history
    "sp" 'consult-ripgrep
    ;;local leader
    "mm" 'consult-mode-command))

(use-package embark-consult)

;; Allows you to insert directories into the minibuffer prompt. It integrates
;; with the minibuffer history, so it gets smarter the more you use it.
(use-package consult-dir
  :general
  (:keymaps 'vertico-map
            "M-d" 'consult-dir))

(use-package consult-eglot
  :after eglot
  :general
  (rex-leader
    "cs" 'consult-eglot-symbols))
