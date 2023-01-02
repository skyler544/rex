;; TODO Set this up; should make it possible to register commands and
;; select them via completion
(use-package run-command)

;; Terminal emulation
(use-package vterm
  :commands vterm)

(use-package eat
  :config
  (setq eat-term-name "rxvt-unicode-256color")
  (setq eshell-visual-commands nil)
  (eat-eshell-mode))


;; Keep this around just in case, but it seems like overkill to me
;; (use-package run-stuff
;;   :general
;;   (rex-leader
;;     "R" 'run-stuff-command-on-region-or-line))
