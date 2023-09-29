;;; doom-tomorrow-night-alt-theme.el --- One of the dark variants of Tomorrow -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 14, 2017 (4c981f2cccf3)
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Maintainer:
;; Source: https://github.com/ChrisKempson/Tomorrow-Theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tomorrow-night-alt-theme nil
  "Options for the `doom-tomorrow-night-alt' theme."
  :group 'doom-themes)

(defcustom doom-tomorrow-night-alt-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tomorrow-night-alt-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tomorrow-night-alt
  "A theme based off of Chris Kempson's Tomorrow Dark."

  ;; name        gui       256       16
  ((bg         '("#1f2125" nil       nil          ))
   (bg-alt     '("#161721" nil       nil          ))
   (base0      '("#0d0d0f" "black"   "black"      ))
   (base1      '("#1b1b1d" "#1b1b1b"              ))
   (base2      '("#212124" "#1e1e1e"              ))
   (base3      '("#292b2d" "#292929" "brightblack"))
   (base4      '("#3f4042" "#3f3f3f" "brightblack"))
   (base5      '("#5c5e60" "#525252" "brightblack"))
   (base6      '("#75787a" "#6b6b6b" "brightblack"))
   (base7      '("#969898" "#979797" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   (fg         '("#c5c8c6" "#c5c5c5" "white"))
   (fg-alt     (doom-darken fg 0.4))

   (grey       base6)
   (red        '("#cc6666" "#cc6666" "red"))
   (orange     '("#de935f" "#dd9955" "brightred"))
   (yellow     '("#f0c674" "#f0c674" "yellow"))
   (green      '("#b5bd68" "#b5bd68" "green"))
   (blue       '("#81a2be" "#88aabb" "brightblue"))
   (dark-blue  '("#41728e" "#41728e" "blue"))
   (teal       blue) ; FIXME replace with real teal
   (magenta    '("#c9b4cf" "#c9b4cf" "magenta"))
   (violet     '("#b294bb" "#b294bb" "brightmagenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))

   (grey-blue  '("#5d6d7d" "#5d6d7d" nil ))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (selection      (doom-darken (doom-blend base4 dark-blue 0.2) 0.1))
   (builtin        blue)
   (comments       (doom-blend grey blue 0.8))
   (doc-comments   (doom-lighten grey 0.14))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg    (doom-darken (doom-blend bg-alt blue 0.2) 0.6))
   (modeline-bg-alt (doom-darken (doom-blend bg-alt blue 0.2) 0.8))
   (modeline-fg     fg)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-tomorrow-night-alt-padded-modeline
      (if (integerp doom-tomorrow-night-alt-padded-modeline)
          doom-tomorrow-night-alt-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((line-number &override) :foreground comments)
   ((line-number-current-line &override) :foreground blue :bold bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ((hl-line &override) :background (doom-darken grey-blue 0.4))
   ((homoglyph &override) :foreground cyan)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path))

  ;; --- variables --------------------------
  ;; ()
  )

;;; doom-tomorrow-night-alt-theme.el ends here
