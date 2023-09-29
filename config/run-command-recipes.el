;;; run-command-recipes.el --- Recipes for run-command -*- lexical-binding: t -*-

;;; Commentary:

;; Recipes for makefile targets and package.json scripts.

;;; Code:

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

(provide 'run-command-recipes)
;;; run-command-recipes.el ends here
