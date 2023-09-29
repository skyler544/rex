(use-package emacs :elpaca nil
  :config
  (defun rex/frame-init (face fam size)
    (set-face-attribute face nil
                        :family fam
                        :height size))

  (defun rex/frame-init-aux (fam size)
    (let ((faces '(default fixed-pitch variable-pitch)))
      (dolist (face faces)
        (rex/frame-init face fam size))))

  (defun rex/frame-init-iosevka ()
    (interactive)
      (rex/frame-init-aux "Iosevka Custom" 120))
  (defun rex/frame-init-input ()
    (interactive)
      (rex/frame-init-aux "Input Mono Narrow" 120))
  (defun rex/frame-init-ibm ()
    (interactive)
      (rex/frame-init-aux "IBM Plex Mono" 120))
  (defun rex/frame-init-inconsolata ()
    (interactive)
      (rex/frame-init-aux "Inconsolata" 120))
  (defun rex/frame-init-fira ()
    (interactive)
      (rex/frame-init-aux "Fira Code" 120))
  (defun rex/frame-init-jm ()
    (interactive)
      (rex/frame-init-aux "JuliaMono" 120))
  (defun rex/frame-init-consolas ()
    (interactive)
      (rex/frame-init-aux "Consolas" 120))
  (defun rex/frame-init-commit ()
    (interactive)
      (rex/frame-init-aux "Commit Mono" 120))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (rex/frame-init-commit)))
    (rex/frame-init-commit)))
