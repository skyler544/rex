;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Configuration of shell-related tools
;; ----------------------------------------------------


;; Improvements for built-in shell modes
;; ----------------------------------------------------
(use-package emacs
  :custom-face
  (ansi-color-bright-black
   ((t ( :background unspecified
	     :foreground unspecified))))
  :config
  (setq async-shell-command-display-buffer nil)
  (add-hook 'term-exec-hook
	        (function
	         (lambda ()
	           (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

;; This package improves the use of the built-in shell mode.
(use-package shell-command-x
  :config
  (shell-command-x-mode 1))


;; Terminal emulation
;; ----------------------------------------------------
(use-package eat
  :general
  (rex-leader
    "ot" 'eat
    "pt" 'eat-project)
  (:states 'normal
           :keymaps 'eat-semi-char-mode-map
           "RET" 'eat-self-input
           "p" 'eat-yank)
  :config
  (setq eat-term-scrollback-size nil)
  (add-to-list 'evil-insert-state-modes 'eat-mode))


;; Command runner for project build systems
;; ----------------------------------------------------
(use-package run-command
  :general
  (rex-leader
    "R" 'run-command)
  :config
  (require 'subr-x)
  (require 'map)
  (require 'seq)

  (defun run-command-recipe-package-json ()
    "Provide commands to run script from `package.json'.

Automatically detects package manager based on lockfile: npm, yarn, and pnpm."
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "package.json"))
                (project-info
                 (with-temp-buffer
                   (insert-file-contents
                    (concat project-dir "package.json"))
                   (json-parse-buffer)))
                (package-manager
                 (cond
                  ((file-exists-p
                    (concat project-dir "pnpm-lock.yaml"))
                   "pnpm")
                  ((file-exists-p
                    (concat project-dir "yarn.lock"))
                   "yarn")
                  (t
                   "npm")))
                (scripts (map-keys (map-elt project-info "scripts"))))
      (seq-map
       (lambda (script)
         (list
          :command-name script
          :command-line (concat package-manager " run " script)
          :display script
          :working-dir project-dir))
       scripts)))

  (defun run-command-recipe-make ()
    "Provide commands to run Makefile targets."
    (require 'make-mode)
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "Makefile"))
                (makefile (concat project-dir "Makefile"))
                (targets (-map #'car
                               (with-current-buffer
                                   (find-file-noselect makefile t)
                                 (setq makefile-need-target-pickup t)
                                 (makefile-pickup-targets)
                                 makefile-target-table))))
      (seq-map
       (lambda (target)
         (list
          :command-name target
          :command-line (concat "make " target)
          :display target
          :working-dir project-dir))
       targets)))
  (setq run-command-default-runner 'run-command-runner-eat)
  (setq run-command-recipes '(run-command-recipe-package-json run-command-recipe-make)))
