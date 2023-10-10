;;; -*- lexical-binding: t -*-
;;
;; Global variables and helper functions
;; ----------------------------------------------------
(setq *rex/config-dir* (expand-file-name "config/" user-emacs-directory))
(setq *rex/theme-dir* (expand-file-name "themes/" user-emacs-directory))
(setq *rex/var-dir* (expand-file-name "var/" user-emacs-directory))
(setq *rex/etc-dir* (expand-file-name "etc/" user-emacs-directory))

(defun rex/load (file)
  (load (expand-file-name file *rex/config-dir*)))

(defun rex/idle-load (file seconds)
  (run-with-idle-timer
   seconds nil #'(lambda () (rex/load file))))


;; General settings
;; ----------------------------------------------------
(rex/load "rex-base")
(rex/load "rex-functions")
(rex/load "rex-display-buffer-setup")


;; Keybind system
;; ----------------------------------------------------
(rex/load "rex-keybinds")
(rex/load "rex-hydra")


;; Help and utilities
;; ----------------------------------------------------
(rex/load "rex-helpers")
(rex/load "rex-shell-stuff")
(rex/load "rex-movec")
(rex/load "rex-completion")


;; Editing
;; ----------------------------------------------------
(rex/load "rex-editing")


;; Languages
;; ----------------------------------------------------
(rex/load "rex-lang-support")
(rex/load "rex-lang")


;; Themes and UI
;; ----------------------------------------------------
(rex/load "rex-ui")


;; Org
;; ----------------------------------------------------
(rex/idle-load "rex-org" 15)


;; Miscellaneous
;; ----------------------------------------------------
(rex/load "rex-miscellaneous")


;; Email
;; ----------------------------------------------------
(defun rex/mu4e ()
  (interactive)
  (load "~/build/rex-email/rex-email.el")
  (mu4e))
