(use-package eglot
  :commands (eglot))

(use-package eglot-java
  :commands (eglot-java-mode))

(use-package lua-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))
