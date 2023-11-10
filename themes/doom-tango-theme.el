;;; doom-tango-theme.el --- an original light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright NIL
;;
;; Authors: Skyler Mayfield <skyler544@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tango-theme nil
  "Options for the `doom-tango' theme."
  :group 'doom-themes)

(defcustom doom-tango-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-tango-theme
  :type 'boolean)

(defcustom doom-tango-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-tango-theme
  :type 'boolean)

(defcustom doom-tango-comment-bg doom-tango-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-tango-theme
  :type 'boolean)

(defcustom doom-tango-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tango-theme
  :type '(choice integer boolean))

(defcustom doom-tango-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-tango-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-tango
  "A doom theme based on the built-in tango theme."

  ;; name        default   256       16
  ((bg         '("#f4f4f4" nil       nil ))
   (bg-alt     '("#eeeeef" nil       nil ))
   (base0      '("#fafafa" "#dfdfdf" nil ))
   (base1      '("#f5f5f5" "#979797" nil ))
   (base2      '("#eeeeee" "#6b6b6b" nil ))
   (base3      '("#e0e0e5" "#525252" nil ))
   (base4      '("#B8C5DB" "#3f3f3f" "brightblack"))
   (base5      '("#AEBACF" "#525252" "brightblack"))
   (base6      '("#A1ACC0" "#6b6b6b" "brightblack"))
   (base7      '("#60728C" "#979797" "brightblack"))
   (base8      '("#485163" "#dfdfdf" "white"))
   (fg         '("#2e3436" "#2a2a2a" nil ))
   (fg-alt     '("#454545" "#757575" nil ))
   (alum-2     '("#d3d7cf" "#d3d7cf" nil))
   (alum-5     '("#5f615c" "#5f615c" nil))

   (grey       base4)
   (red        '("#a40000" "#ff6655" nil ))
   (orange     '("#ce5c00" "#dd8844" nil ))
   (green      '("#346604" "#99bb66" nil ))
   (teal       '("#29838d" "#44b9b1" nil ))
   (yellow     '("#9a7500" "#ECBE7B" nil ))
   (blue       '("#3465a4" "#51afef" nil ))
   (dark-blue  '("#204a87" "#2257A0" nil ))
   (magenta    '("#75507b" "#c678dd" nil ))
   (violet     '("#5c3566" "#a9a1e1" nil ))
   (cyan       '("#398eac" "#46D9FF" nil ))
   (dark-cyan  '("#2c7088" "#5699AF" nil ))

   (grey-blue  '("#5d6d7d" "#5d6d7d" nil ))
   (bruise         (doom-blend alum-5 violet 0.6))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       alum-5)
   (doc-comments   bruise)
   (constants      dark-blue)
   (functions      red)
   (keywords       green)
   (methods        red)
   (operators      blue)
   (type           dark-blue)
   (strings        violet)
   (variables      orange)
   (numbers        dark-blue)
   (region         (doom-lighten grey-blue 0.6))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   (cursor         dark-blue)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-tango-brighter-modeline)
   (-modeline-pad
    (when doom-tango-padded-modeline
      (if (integerp doom-tango-padded-modeline) doom-tango-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base7)

   (shadow base3)

   (modeline-bg alum-2)
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive (doom-lighten modeline-bg 0.2))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.1)))

  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-tango-comment-bg (doom-lighten bg 0.05)))
   (lazy-highlight :background (doom-blend bg highlight 0.7) :weight 'bold)
   ((line-number &override) :foreground bruise)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ((hl-line &override) :background (doom-lighten grey-blue 0.8))

   ((font-lock-constant-face &override) :weight 'bold)
   ((font-lock-number-face &override) :weight 'bold)
   ((minibuffer-prompt &override) :weight 'bold :foreground dark-blue)

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

;;; doom-tango-theme.el ends here
