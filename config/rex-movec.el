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
  ("C-," 'embark-act)
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
    ;; goto
    "gg" 'consult-goto-line
    "gm" 'consult-mark
    "gk" 'consult-global-mark
    ;; project
    "pb" 'consult-project-buffer
    ;; search
    "sd" 'consult-ripgrep
    "si" 'consult-imenu
    "sI" 'consult-imenu-multi
    "sl" 'consult-line
    "se" 'consult-isearch-history
    ;;local leader
    "mm" 'consult-mode-command))

;; Allows you to insert directories into the minibuffer prompt. It
;; integrates with the minibuffer history, so it gets smarter the more
;; you use it.
(use-package consult-dir
  :general
  (:keymaps 'vertico-map
            "M-d" 'consult-dir))
