;;; doom-opera-light-alt-theme.el --- an original light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 16, 2018 (#189)
;; Author: jwintz <https://github.com/jwintz>
;; Maintainer:
;; Source: original
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-opera-light-alt-theme nil
  "Options for the `doom-opera-light-alt' theme."
  :group 'doom-themes)

(defcustom doom-opera-light-alt-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-opera-light-alt-theme
  :type 'boolean)

(defcustom doom-opera-light-alt-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-opera-light-alt-theme
  :type 'boolean)

(defcustom doom-opera-light-alt-comment-bg doom-opera-light-alt-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-opera-light-alt-theme
  :type 'boolean)

(defcustom doom-opera-light-alt-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-opera-light-alt-theme
  :type '(choice integer boolean))

(defcustom doom-opera-light-alt-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-opera-light-alt-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-opera-light-alt
  "A light Opera theme."

  ;; name        default   256       16
  ((bg         '("#fdfdfd" nil       nil ))
   (bg-alt     '("#eeeeee" nil       nil ))
   (base0      '("#fafafa" "#dfdfdf" nil ))
   (base1      '("#f5f5f5" "#979797" nil ))
   (base2      '("#eeeeee" "#6b6b6b" nil ))
   (base3      '("#e0e0e0" "#525252" nil ))
   (base4      '("#B8C5DB" "#3f3f3f" "brightblack"))
   (base5      '("#AEBACF" "#525252" "brightblack"))
   (base6      '("#A1ACC0" "#6b6b6b" "brightblack"))
   (base7      '("#60728C" "#979797" "brightblack"))
   (base8      '("#485163" "#dfdfdf" "white"))
   (fg         '("#2a2a2a" "#2a2a2a" nil ))
   (fg-alt     '("#454545" "#757575" nil ))

   (grey       base4)
   (red        '("#99324b" "#ff6655" nil ))
   (orange     '("#ac4426" "#dd8844" nil ))
   (green      '("#4f894c" "#99bb66" nil ))
   (teal       '("#29838d" "#44b9b1" nil ))
   (yellow     '("#9a7500" "#ECBE7B" nil ))
   (blue       '("#3b6ea8" "#51afef" nil ))
   (dark-blue  '("#5272AF" "#2257A0" nil ))
   (magenta    '("#97365b" "#c678dd" nil ))
   (violet     '("#842879" "#a9a1e1" nil ))
   (cyan       '("#398eac" "#46D9FF" nil ))
   (dark-cyan  '("#2c7088" "#5699AF" nil ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (doom-darken base6 0.2))
   (doc-comments   (doom-lighten (if doom-opera-light-alt-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      magenta)
   (numbers        magenta)
   (region         (doom-lighten base4 0.2))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-opera-light-alt-brighter-modeline)
   (-modeline-pad
    (when doom-opera-light-alt-padded-modeline
      (if (integerp doom-opera-light-alt-padded-modeline) doom-opera-light-alt-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.1)))

  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-opera-light-alt-comment-bg (doom-lighten bg 0.05)))
   (lazy-highlight :background (doom-blend bg highlight 0.7) :weight 'bold)
   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; ivy
   (ivy-current-match :background base3)
   ;;;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-opera-light-alt-theme.el ends here
