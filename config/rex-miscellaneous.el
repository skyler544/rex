(use-package emacs
  :config
  (defun rex/set-capf-nonexclusive (local-capf)
    (setq-local
     completion-at-point-functions
     (list (cape-capf-properties local-capf :exclusive 'no)))))
