;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Joel Dueck"
      user-mail-address "joel@jdueck.net")

;; Terminex is an IBM Plex Mono that includes box/line characters and slashed zero
;; https://github.com/vsalvino/terminex
(defvar velcro-fonts '("Terminex" "Rec Mono SemiCasual" "Triplicate T4c"))

(defvar velcro-themes '(twilight-bright almost-mono-white doom-one))
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
(setq doom-font (font-spec :family (car velcro-fonts) :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (car velcro-themes))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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

;; Linting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq-default
   flycheck-standard-error-navigation nil  ;; prevent flycheck from rebinding next-error (M-g n)
   flycheck-disabled-checkers '(python-pycompile racket sass scheme-chicken)
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-flake8rc "setup.cfg"))

(use-package! racket-mode
  :mode "\\.rkt\\'"
  :after flycheck
  :preface
  (defun bp-insert-lisp-section (section)
    "Insert a LISP section header with SECTION at point."
    (interactive "sSection: ")
    (let ((suffix (s-repeat (- 72 (length section) 4) ";")))
      (insert (format ";; %s %s\n" section suffix))))
  (defvar bp-racket-defun-likes
    '(call-with-browser!
      call-with-browser-script!
      call-with-database-connection
      call-with-database-transaction
      call-with-element-screenshot!
      call-with-input-bytes
      call-with-input-string
      call-with-marionette!
      call-with-page!
      call-with-page-screenshot!
      call-with-persistent-database-connection
      call-with-pk
      call-with-pool-connection
      call-with-postmark-connection
      call-with-pubsub-events
      call-with-redis
      call-with-redis-client
      call-with-redis-pool
      call-with-redis-pubsub
      call-with-screenshot
      call-with-semaphore
      call-with-test-client+server
      call-with-transaction
      call-with-twilio-connection
      call-with-unzip
      for/stream
      form*
      gen:let
      let*
      let-globals
      place
      property
      section
      serializable-struct
      serializable-struct/versions
      struct++
      system-test-suite
      test
      test-commands
      tpl:xexpr-when
      xexpr-unless
      xexpr-when))

  (defun bp-racket-mode-hook ()
    (interactive)
    (setq adaptive-fill-mode t))
  :config
  (add-hook 'racket-mode-hook #'bp-racket-mode-hook)

  (flycheck-define-checker racket-review
    "check racket source code using racket-review"
    :command ("raco" "review" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
    :modes racket-mode)

  (add-to-list 'flycheck-checkers 'racket-review)

  (setq racket-repl-buffer-name-function #'racket-repl-buffer-name-project
        racket-show-functions '(racket-show-echo-area))

  (dolist (id bp-racket-defun-likes)
    (put id 'racket-indent-function #'defun))

  :bind (:map racket-mode-map
              ("{"       . paredit-open-curly)
              ("}"       . paredit-close-curly)
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c C-s" . bp-insert-lisp-section)
              ("C-c ."   . racket-xp-visit-definition)
              ("C-c ,"   . racket-unvisit)))

(use-package racket-xp-mode
  :hook racket-mode)

(use-package! diff-hl
  :config
  (global-diff-hl-mode))

;; I can never get these to work quite properly when I try to “defer” them in any way
;(use-package! pollen-mode)
;(use-package! company-pollen)

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

(defun make-cycler (lst func)
  (let ((counter 0))
    (function (lambda ()
      (setq counter (1+ counter))
      (funcall func (nth (mod counter (length lst)) lst))))))

(defvar velcro-fontspecs
  (mapcar (lambda (f) (font-spec :family f :weight 'normal)) velcro-fonts))

(defvar cycle-font (make-cycler velcro-fontspecs #'set-frame-font))
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
