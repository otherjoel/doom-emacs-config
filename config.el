;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Joel Dueck"
      user-mail-address "joel@jdueck.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "IBM Plex Mono" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! racket-mode
  :mode "\\.rkt\\'"
  :magic "#lang racket"
  :config
  (company-mode)
  (rainbow-delimiters-mode))

(use-package! diff-hl
  :config
  (global-diff-hl-mode))

;; I can never get these to work quite properly when I try to “defer” them in any way
(use-package! pollen-mode)
(use-package! company-pollen)

(setq initial-frame-alist '((top . 0) (left . 0) (width . 106) (height . 90)))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(doom-modeline-buffer-modified :foreground "orange"))

(blink-cursor-mode 1)

(setq-default fill-column 100)

;; Restore Mac OS keybindings for proper punctuation in insert mode
(map!
 :i "M-[" "“"
 :i "M-{" "”"
 :i "M-]" "‘"
 :i "M-}" "’"
 :i "M--" "–"
 :i "M-_" "—"
 :i "M-;" "…")

(defvar velcro-fonts '("Rec Mono SemiCasual" "IBM Plex Mono" "Triplicate T4c"))
(defvar velcro-themes '(doom-one almost-mono-white))

(defun make-cycler (lst func)
  (let ((counter 0))
    (function (lambda ()
      (setq counter (1+ counter))
      (funcall func (nth (mod counter (length lst)) lst))))))

(defvar cycle-font (make-cycler velcro-fonts #'set-frame-font))
(defvar cycle-theme (make-cycler velcro-themes #'load-theme))

(defun velcro-cycle-font () (interactive) (funcall cycle-font))
(defun velcro-cycle-theme () (interactive) (funcall cycle-theme))

(map! :leader
      :desc "Cycle font"
      "t F" #'velcro-cycle-font
      :desc "Cycle theme"
      "t T" #'velcro-cycle-theme)

(defun scribble-render-and-open ()
  "Render Scribble document to HTML and open in browser"
  (interactive)
  (shell-command
   (format "scribble +m --html %s && open %s.html"
           (shell-quote-argument (buffer-file-name))
           (file-name-sans-extension (buffer-file-name))))
  )

(map! :map scribble-mode-map
      :leader
      :desc "Scribble → HTML & open"
      :n "r h" #'scribble-render-and-open)
