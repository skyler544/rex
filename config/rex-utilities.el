;; TODO Set this up; should make it possible to register commands and
;; select them via completion
(use-package run-command)

;; Terminal emulation
(use-package vterm
  :general
  (rex-leader
    "ot" 'vterm)
  :commands vterm)

(use-package eat
  :config
  ;; why doesn't terminfo work here?
  (setq eat-term-name "rxvt-unicode-256color")
  (setq eshell-visual-commands nil)
  (eat-eshell-mode))

(use-package docker
  :general
  (rex-leader
    "mD" 'docker))

(use-package docker-tramp)

(use-package bufler)

(use-package burly
  :general
  (rex-leader
    "bB" 'burly-bookmark-windows))

(use-package dash)

;; Keep this around just in case, but it seems like overkill to me
;; (use-package run-stuff
;;   :general
;;   (rex-leader
;;     "R" 'run-stuff-command-on-region-or-line))
