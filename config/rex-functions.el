;;; -*- lexical-binding: t -*-
;;
;; ----------------------------------------------------
;; Custom elisp functions, some original, some stolen.
;; ----------------------------------------------------


;; Buffers
;; ----------------------------------------------------
(use-package emacs
  :config
  (defun rex/ansi-color-apply-on-region (begin end)
    (interactive "r")
    (ansi-color-apply-on-region begin end t))

  (defun rex/buffer-info ()
    "Briefly describe the current buffer."
    (interactive)
    (message (concat "Name:\t" (buffer-name)
                     "\nFile:\t"
                     (if (buffer-file-name)
                         (buffer-file-name)
                       "no file")
                     "\nSize:\t" (int-to-string (buffer-size)))))

  (defun rex/kill-relative-path ()
    "Kill the path to the current project relative to the project root."
    (interactive)
    (kill-new (file-relative-name buffer-file-name (project-root (project-current t)))))

  (defun rex/large-file-read-only ()
    "If a file is over a given size, make the buffer read only (and don't waste memory trying to use undo)"
    (when (> (buffer-size) (* 1024 1024))
      (setq buffer-read-only t)
      (buffer-disable-undo)))
  :hook (find-file . rex/large-file-read-only))


;; Shell commands
;; ----------------------------------------------------
(use-package emacs
  :config
  (defun rex/async-shell-command-on-region-or-line ()
    "Run the command at point or in the selected region in the shell."
    (interactive)
    (async-shell-command (if (use-region-p)
                             (buffer-substring (region-beginning) (region-end))
                           (thing-at-point 'line t))))

  (defun rex/shell-command-on-region-or-line ()
    "Run the command at point or in the selected region in the shell."
    (interactive)
    (shell-command (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     (thing-at-point 'line t)))))


;; System services
;; ----------------------------------------------------
(use-package emacs
  :config
  (defun rex/service-command (state service)
    (let ((default-directory "/sudo::"))
      (shell-command (format "sv %s %s" state service))))
  (defun rex/service-up (service)
    (rex/service-command "up" service))
  (defun rex/service-down (service)
    (rex/service-command "down" service))

  (defun rex/fh-vpn-up ()
    "Start the FH OpenVPN connection."
    (interactive)
    (rex/service-up "fh-vpn"))

  (defun rex/fh-vpn-down ()
    "Stop the FH OpenVPN connection."
    (interactive)
    (rex/service-down "fh-vpn"))

  (defun rex/touchegg-up ()
    "Start touchegg."
    (interactive)
    (rex/service-up "touchegg"))

  (defun rex/touchegg-down ()
    "Stop touchegg."
    (interactive)
    (rex/service-down "touchegg"))

  (defun rex/wpa_supplicant-up ()
    "Start wpa_supplicant."
    (interactive)
    (rex/service-up "wpa_supplicant"))

  (defun rex/wpa_supplicant-down ()
    "Stop wpa_supplicant."
    (interactive)
    (rex/service-down "wpa_supplicant"))

  (defun rex/NetworkManager-up ()
    "Start NetworkManager."
    (interactive)
    (rex/service-up "NetworkManager"))

  (defun rex/NetworkManager-down ()
    "Stop NetworkManager."
    (interactive)
    (rex/service-down "NetworkManager"))

  (defun rex/cinnamon ()
    "Setup cinnamon services."
    (interactive)
    (rex/wpa_supplicant-down)
    (rex/NetworkManager-up)
    (rex/touchegg-up))

  (defun rex/awesome ()
    "Setup awesome services."
    (interactive)
    (rex/NetworkManager-down)
    (rex/wpa_supplicant-up)
    (rex/touchegg-down))

  (defun rex/docker-up ()
    "Start docker"
    (interactive)
    (let ((default-directory "/sudo::"))
      (shell-command "sv up docker")))

  (defun rex/docker-down ()
    "Stop docker"
    (interactive)
    (let ((default-directory "/sudo::"))
      (shell-command "sv down docker")))

  (defun rex/docker-compose-up ()
    "Run docker-compose up -d in the docker-local directory of the current project."
    (interactive)
    (async-shell-command (concat "docker-compose -f "
                                 (project-root (project-current t))
                                 "docker-local/docker-compose.yml up -d")))

  (defun rex/docker-compose-down ()
    "Run docker-compose down in the docker-local directory of the current project."
    (interactive)
    (async-shell-command (concat "docker-compose -f "
                                 (project-root (project-current t))
                                 "docker-local/docker-compose.yml down"))))


;; Theme stuff
;; ----------------------------------------------------
(use-package emacs
  :config
  (defun rex/clean-load-theme (theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

  (defun rex/dark-theme ()
    "Switch to a dark theme."
    (interactive)
    ;; (rex/clean-load-theme 'doom-spacegrey-alt)
    (rex/clean-load-theme 'doom-tomorrow-night-alt))

  (defun rex/light-theme ()
    "Switch to a light theme."
    (interactive)
    (rex/clean-load-theme 'doom-tango)))


;; Window manipulation
;; ----------------------------------------------------
(use-package emacs
  :config
  ;; window movement; stolen from doom
  (defun rex--window-swap (direction)
    "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
    (when (window-dedicated-p)
      (user-error "Cannot swap a dedicated window"))
    (let* ((this-window (selected-window))
           (this-buffer (current-buffer))
           (that-window (windmove-find-other-window direction nil this-window))
           (that-buffer (window-buffer that-window)))
      (when (or (minibufferp that-buffer)
                (window-dedicated-p this-window))
        (setq that-buffer nil that-window nil))
      (if (not (or that-window (one-window-p t)))
          (funcall (pcase direction
                     ('left  #'evil-window-move-far-left)
                     ('right #'evil-window-move-far-right)
                     ('up    #'evil-window-move-very-top)
                     ('down  #'evil-window-move-very-bottom)))
        (unless that-window
          (setq that-window
                (split-window this-window nil
                              (pcase direction
                                ('up 'above)
                                ('down 'below)
                                (_ direction))))
          (setq that-buffer (window-buffer that-window)))
        (window-swap-states this-window that-window)
        (select-window that-window))))

  (defun rex/window-move-left ()
    "Swap windows to the left."
    (interactive) (rex--window-swap 'left))
  (defun rex/window-move-down ()
    "Swap windows downward."
    (interactive) (rex--window-swap 'down))
  (defun rex/window-move-up ()
    "Swap windows upward."
    (interactive) (rex--window-swap 'up))
  (defun rex/window-move-right ()
    "Swap windows to the right"
    (interactive) (rex--window-swap 'right))

  ;; override window split behavior
  (defun rex/split-and-follow-horizontally ()
    "Split window below and place point in the new window."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun rex/split-and-follow-vertically ()
    "Split window right and place point in the new window."
    (interactive)
    (split-window-right)
    (other-window 1)))
