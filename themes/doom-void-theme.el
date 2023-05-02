;;; doom-void-theme.el ---  -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-void-theme nil
  "Options for the `doom-void' theme."
  :group 'doom-themes)

(defcustom doom-void-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-void-theme
  :type 'boolean)

(defcustom doom-void-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-void-theme
  :type 'boolean)

(defcustom doom-void-comment-bg doom-void-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhances their legibility."
  :group 'doom-void-theme
  :type 'boolean)

(defcustom doom-void-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-void-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-void
  "A theme inspired by Void linux"

  ;; name        default   256       16
  ((bg         '("#252525" nil       nil            )) ;; void-bg
   (bg-alt     '("#222222" nil       nil            )) ;; vid-bg-05
   (base0      '("#000000" "black"   "black"        )) ;; void-bg-2
   (base1      '("#2B2B2B" "#1e1e1e" "brightblack"  )) ;; void-bg-1
   (base2      '("#303030" "#2e2e2e" "brightblack"  )) ;; void-bg-08
   (base3      '("#383838" "#262626" "brightblack"  )) ;; void-bg-05
   (base4      '("#494949" "#3f3f3f" "brightblack"  )) ;; void-bg+05
   (base5      '("#4F4F4F" "#525252" "brightblack"  )) ;; void-bg+1
   (base6      '("#5F5F5F" "#6b6b6b" "brightblack"  )) ;; void-bg+2
   (base7      '("#6F6F6F" "#979797" "brightblack"  )) ;; void-bg+3
   (base8      '("#FFFFEF" "#dfdfdf" "white"        )) ;; void-fg+1
   (fg         '("#DCDCDC" "#bfbfbf" "brightwhite"  )) ;; void-fg
   (fg-alt     '("#989890" "#2d2d2d" "white"        )) ;; void-fg-05

   (grey       base4)
   (red        '("#be443f" "#ff6655" "red"          )) ;; void-red
   (orange     '("#dd8844" "#dd8844" "brightred"    )) ;; void-orange
   (green      '("#7FA27F" "#99bb66" "green"        )) ;; void-green
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  )) ;; void-??
   (yellow     '("#c0aB6B" "#ECBE7B" "yellow"       )) ;; void-yellow
   (blue       '("#8CD0D3" "#51afef" "brightblue"   )) ;; void-blue
   (dark-blue  '("#2257A0" "#2257A0" "blue"         )) ;; void-??
   (magenta    '("#c792ea" "#c678dd" "brightmagenta")) ;; void-magenta
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      )) ;; zendurn-??
   (cyan       '("#88b9c9" "#46D9FF" "brightcyan"   )) ;; void-cyan
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         )) ;; void-??

   ;; Extra void colors
   (fg-1       '("#656555"))
   (fg+2       '("#FFFFFD"))
   (red-4      '("#7e443f"))
   (red-1      '("#ae443f"))
   (red+1      '("#ce443f"))
   (yellow-2   '("#a08B4B"))
   (yellow-1   '("#c09B5B"))
   (green-2    '("#5F7F5F"))
   (green+1    '("#8FB28F"))
   (green+2    '("#9FC59F"))
   (green+3    '("#AFD8AF"))
   (green+4    '("#BFEBBF"))
   (blue+1     '("#94BFF3"))
   (blue-1     '("#7CB8BB"))
   (blue-2     '("#6CA0A3"))
   (blue-3     '("#5C888B"))
   (blue-4     '("#4C7073"))
   (blue-5     '("#366060"))
   (gray       '("#4f5b66"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        red)
   (comments       green+1)
   (doc-comments   green+2)
   (constants      red)
   (functions      cyan)
   (keywords       green)
   (methods        blue-2)
   (operators      red)
   (type           yellow)
   (strings        violet)
   (variables      dark-cyan)
   (numbers        red)
   (region         (doom-darken green 0.4))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-void-brighter-modeline)
   (-modeline-pad
    (when doom-void-padded-modeline
      (if (integerp doom-void-padded-modeline) doom-void-padded-modeline 4)))

   (modeline-fg     green+1)
   (modeline-fg-alt `(,(car fg-alt) ,@(cdr base6)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      bg))
   (modeline-bg-inactive   `(,(doom-lighten (car modeline-bg) 0.05) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;;;; Base theme face overrides
  (
   ;; (cursor :foreground fg :background base8)
   (cursor :background blue-2)
   (escape-glyph :foreground yellow :weight 'bold)
   (highlight :background base3)
   (isearch :foreground yellow-2 :weight 'bold :background base6)
   (isearch-fail :foreground fg :background red-4)
   (lazy-highlight :foreground yellow-2 :weight 'bold :background base3)
   ((line-number &override) :foreground base7 :slant 'normal)
   ((line-number-current-line &override) :foreground yellow-2 :slant 'normal)
   (link :foreground yellow-2 :underline t :weight 'bold)
   (minibuffer-prompt :foreground yellow)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (success :foreground green :weight 'bold)
   (tooltip :foreground fg :background base5)
   (vertical-border :foreground fg-1) ;; different
   (warning :foreground orange :weight 'bold)
   (widget-field :foreground fg :background base7)

   ;;;; compilation
   (compilation-error-face :inherit error :underline t)
   (compilation-info :foreground blue)
   (compilation-line-number :foreground yellow)
   (compilation-warning-face :foreground yellow)
   (compilation-mode-line-exit :foreground green+2 :weight 'bold)
   ;;;; calfw
   (cfw:face-default-content :foreground green)
   (cfw:face-disable :foreground fg-1)
   (cfw:face :inherit 'shadow)
   (cfw:face :inherit 'font-lock-keyword-face)
   (cfw:face-sunday :foreground red :weight 'bold)
   (cfw:face :inherit 'cfw:face-sunday)
   (cfw:face-periods :foreground cyan)
   (cfw:face-select :background blue-5)
   (cfw:face-saturday :foreground blue :weight 'bold)
   (cfw:face-select :background blue-5)
   (cfw:face-title :height 2.0 :inherit '(variable-pitch font-lock-keyword-face))
   (cfw:face-today :foreground cyan :weight 'bold)
   (cfw:face-toolbar-button-off :underline nil :inherit 'link)
   (cfw:face-toolbar-button-on :underline nil :inherit 'link-visited)
   ;;;; company
   (company-tooltip-selection  :background base6)
   (company-scrollbar-fg       :background base7)
   (company-tooltip-annotation :foreground green :distant-foreground green)
   ;;;; centaur-tabs
   (centaur-tabs-selected :background bg :foreground fg+2)
   (centaur-tabs-unselected :background base1 :foreground fg-alt)
   (centaur-tabs-selected-modified :background bg :foreground orange)
   (centaur-tabs-unselected-modified :background base1 :foreground orange)
   (centaur-tabs-active-bar-face :background yellow)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected-modified :foreground yellow)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected-modified :foreground yellow)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; custom
   (custom-variable-tag :foreground blue :weight 'bold)
   (custom-group-tag :foreground blue :weight 'bold)
   (custom-state :foreground green+4)
   ;;;; diff-hl
   (diff-hl-change :foreground blue :background blue-2)
   (diff-hl-delete :foreground red+1 :background red-1)
   (diff-hl-insert :foreground green+1 :background green-2)
   ;;;; doom-modeline
   (doom-modeline-bar :background yellow)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; fill-column
   (fill-column-indicator :foreground base4 :weight 'semilight)
   ;;;; flycheck
   (flycheck-error :underline `(:style wave :color ,red-1) :weight 'bold)
   (flycheck-warning :underline `(:style wave :color ,yellow) :weight 'bold)
   (flycheck-info :underline `(:style wave :color ,cyan) :weight 'bold)
   ;;;; git-commit
   (git-commit-comment-action :foreground green+1 :weight 'bold)
   (git-commit-comment-branch :foreground blue+1 :weight 'bold)
   (git-commit-comment-branch-local :foreground blue+1 :weight 'bold)
   ;;;; git-gutter
   (git-gutter:added :foreground green :weight 'bold)
   (git-gutter:deleted :foreground red :weight 'bold)
   (git-gutter:modified :foreground magenta :weight 'bold)
   ;;;; hi-lock
   (hi-green :background green+4 :background base1)
   (hi-green-b :foreground green+2 :weight 'bold)
   ;;;; highlight-symbol
   (highlight-symbol-face :background base6)
   ;;;; highlight-thing
   (highlight-thing :background base6)
   ;;;; helm
   (helm-header :foreground yellow :background base1 :weight 'bold :extend t)
   (helm-source-header :foreground yellow :background base1 :weight 'bold :extend t)
   (helm-selection :background base5)
   (helm-selection-line :background base5)
   (helm-visible-mark :foreground bg :background yellow-2)
   (helm-candidate-number :foreground green+4 :background base1)
   (helm-separator :foreground red :background bg)
   (helm-time-zone-current :foreground green+2 :background bg)
   (helm-time-zone-home :foreground red :background bg)
   (helm-buffer-not-saved :foreground red :background bg)
   (helm-buffer-process :foreground cyan :background bg)
   (helm-buffer-saved-out :foreground fg :background bg)
   (helm-buffer-size :foreground fg-1 :background bg)
   (helm-ff-directory :foreground cyan :weight 'bold)
   (helm-ff-executable :foreground green+2 :background bg :weight 'normal)
   (helm-ff-invalid-symlink :foreground red :background bg :weight 'bold)
   (helm-ff-symlink :foreground yellow :background bg :weight 'bold)
   (helm-ff-prefix :foreground bg :background yellow :weight 'normal)
   (helm-grep-cmd-line :foreground cyan :background bg)
   (helm-grep-file :foreground fg :background bg)
   (helm-grep-finish :foreground green+2 :background bg)
   (helm-grep-lineno :foreground fg-1 :background bg)
   (helm-grep-match :foreground 'nil :background 'nil :inherit 'helm-match)
   (helm-grep-running :foreground red :background bg)
   (helm-match :foreground orange :background base1 :weight 'bold)
   (helm-swoop-target-line-face :foreground fg :background base6)
   (helm-swoop-target-word-face :foreground yellow :background base6 :weight 'bold)
   ;;;; ivy
   (ivy-current-match :background bg-alt :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground green+4 :weight 'bold)
   ;;;; js2-mode
   (js2-jsdoc-tag :foreground green-2)
   (js2-jsdoc-type :foreground green+2)
   (js2-jsdoc-value :foreground green+3)
   (js2-exernal-variable :foreground orange)
   (js2-instance-member :foreground green-2)
   (js2-jsdoc-html-tag-delimiter :foreground orange)
   (js2-jsdoc-html-tag-name :foreground red-1)
   (js2-object-property :foreground blue+1)
   (js2-magic-paren :foreground blue-5)
   (js2-private-function-call :foreground cyan)
   (js2-function-call :foreground cyan)
   (js2-private-member :foreground blue-1)
   (js2-keywords :foreground magenta)
   ;;;; lui
   (lui-time-stampe-face :foreground blue-1)
   (lui-hilight-face :foreground green+2 :background bg)
   (lui-button-face :inherit 'hover-highlight)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; mic-paren
   (paren-face-match    :foreground cyan :background bg :weight 'bold)
   (paren-face-mismatch :foreground bg :background magenta :weight 'bold)
   (paren-face-no-match :foreground bg :background red :weight 'bold)
   ;;;; minibuffer
   (completion-annotations :foreground fg-1)
   ;;;; outline <built-in>
   (outline-1 :foreground orange)
   (outline-2 :foreground green+4)
   (outline-3 :foreground blue-1)
   (outline-4 :foreground yellow-2)
   (outline-5 :foreground cyan)
   (outline-6 :foreground green+2)
   (outline-7 :foreground red-4)
   (outline-8 :foreground blue-4)
   ;;;; rpm-model
   (rpm-doc-face :foreground green)
   (rpm-ghost-face :foreground red)
   (rpm-package-face :foreground red)
   (rpm-package-section-face :foreground yellow)
   ;;;; rst-mode <built-in>
   (rst-level-1-face :foreground orange)
   (rst-level-2-face :foreground green+1)
   (rst-level-3-face :foreground blue-1)
   (rst-level-4-face :foreground yellow-2)
   (rst-level-5-face :foreground cyan)
   (rst-level-6-face :foreground green-2)
   ;;;; solaire-mode
   (solaire-default-face :inherit 'default :background base2)
   (solaire-hl-line-face :inherit 'hl-line :background (doom-darken bg 0.1))
   (solaire-minibuffer-face :inherit 'default :background base2)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; web-mode
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-css-pseudo-class-face :foreground green+3 :weight 'bold)
   (web-mode-css-at-rule-face :foreground orange )
   (web-mode-function-name-face :foreground blue)
   (web-mode-html-attr-value-face :inherit 'font-lock-string-face)
   (web-mode-whitespaces-face :background red)
   ;;;; woman <built-in>
   (woman :inherit 'font-lock-keyword-face)
   (woman :inherit 'font-lock-string-face italic)

   (nav-flash-face :inherit 'region)

   (lsp-modeline-code-actions-face :foreground green+2)

   (org-headline-done :foreground base7)
   (org-document-info-keyword :inherit 'font-lock-comment-face)

   (org-agenda-date-weekend-today :foreground green :weight 'bold)
   (org-agenda-date-today :foreground green :weight 'bold)
   (org-agenda-date-weekend :foreground blue-2 :weight 'semibold)
   (org-agenda-date :foreground (doom-darken fg 0.2) :weight 'semibold)
   )
  ;;;; Base theme variable overrides-
  ())

;;; doom-void-theme.el ends here
