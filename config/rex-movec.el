;;; -*- lexical-binding: t -*-
;;
;; This provides a vertical view of the current candidates while
;; interacting with the minibuffer.
(use-package vertico
  :init
  (vertico-mode)
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 12)
  (setq vertico-resize nil))

;; This extends vertico with subjectively nicer keybindings for
;; navigating directories.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :general
  (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char
            "M-DEL" 'vertico-directory-delete-word)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; This completion style makes "narrowing" from a list of candidates
;; very quick and intuitive. It also provides nice font-faces for
;; visually distinguishing what exactly you're matching on in a
;; string.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file) (styles partial-completion))))

;; This package adds information to the minibuffer that can be used in
;; various ways. The information can be very nice by itself, but it
;; also adds context to certain minibuffer actions which are lacking
;; by default, automatically making the matches more relevant. This is
;; directly used by Embark.
(use-package marginalia
  :init
  (marginalia-mode))

;; A context-aware keyboard-driven equivalent of a right-click
;; menu. This thing is a swiss-army-knife on steroids. Beware; it will
;; make everything better than it was before.
(use-package embark
  :general
  ("C-;" 'embark-act)
  ("C-'" 'embark-act-all)
  ("C-h B" 'embark-bindings)
  (rex-leader
    "a" 'embark-act)
  :config
  (setq embark-indicators '(embark-minimal-indicator))
  (setq embark-prompter 'embark-completing-read-prompter))

;; A very smart package that provides lots of nice functions. Many of
;; them are helpful for searching, but there are also commands to help
;; with jumping between various things and interacting with the kill
;; ring. Combine with Embark and mix well.
(use-package consult
  :config
  (defun rex/consult-ripgrep-in-a-directory ()
    "Query the user for a directory to grep in."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'consult-ripgrep)))
  (defun rex/consult-grep-in-current-directory ()
    "Run consult-grep in the current directory."
    (interactive)
    (consult-ripgrep default-directory))
  (consult-customize consult-themes :preview-key (kbd "M-.")
                     consult-buffer :preview-key (kbd "M-."))

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
  (:keymaps '(normal visual)
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
    "sg" 'rex/consult-ripgrep
    "sd" 'rex/consult-grep-in-current-directory
    "sD" 'rex/consult-ripgrep-in-a-directory
    "si" 'consult-imenu
    "sI" 'consult-imenu-multi
    "sl" 'consult-line
    "se" 'consult-isearch-history
    "sp" 'consult-grep
    ;;local leader
    "mm" 'consult-mode-command))

(use-package embark-consult)

;; Allows you to insert directories into the minibuffer prompt. It
;; integrates with the minibuffer history, so it gets smarter the more
;; you use it.
(use-package consult-dir
  :general
  (:keymaps 'vertico-map
            "M-d" 'consult-dir))

(use-package consult-eglot
  :after eglot
  :general
  (rex-leader
    "cs" 'consult-eglot-symbols))
