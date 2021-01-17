;;; init.el --- Emacs personal startup file
;;
;; External dependencies of my Emacs configuration
;;
;; Copying/pasting to and from clipboard when running in terminal
;; emulator
;; ArchLinux: pacman -Sy wl-clipboard xsel
;;
;; diff for M-x diff, emerge-files and related commands
;; ArchLinux: pacman -Sy diffutils
;; MS Windows: scoop install diffutils
;;
;; EditorConfig C Core for editorconfig
;; ArchLinux: pacman -Sy editorconfig-core-c
;; MS Windows: scoop install editorconfig
;;
;; Flycheck linters for JavaScript, JSON, CSS
;; ArchLinux & MS Windows: npm install -g eslint jsonlint stylelint
;;
;; Flycheck linter for HTML documents
;; ArchLinux: pacman -Sy tidy
;; MS Windows: scoop install tidy
;;
;; Flycheck linter for Rust
;; ArchLinux & MS Windows: rustup component add clippy
;;
;; Git for straight.el package manager & magit
;; ArchLinux: pacman -Sy git
;; MS Windows: scoop install git
;;
;; Hunspell for spellchecking
;; ArchLinux: pacman -Sy hunspell hunspell-en_US hunspell-ru
;; MS Windows: choco install hunspell.portable
;;
;; LSP servers for CSS, HTML, JavaScript, TypeScript, YAML
;; ArchLinux & MS Windows: npm install -g vscode-css-languageserver-bin \
;; vscode-html-languageserver-bin typescript-language-server \
;; typescript yaml-language-server
;;
;; LSP server for Rust
;; ArchLinux & MS Windows: rustup component add rls
;;
;; markdown processor for markdown-mode (used for rendering HTML for preview and export)
;; ArchLinux: pacman -Sy pandoc
;; MS Windows: choco install pandoc
;;
;; Ripgrep for rg.el
;; ArchLinux: pacman -Sy ripgrep
;; MS Windows: scoop install ripgrep
;;
;; rustfmt for rust-mode
;; https://github.com/rust-lang-nursery/rustfmt
;; ArchLinux & MS Windows: rustup component add rustfmt
;;
;; gzip for building tree-sitter-langs
;; ArchLinux: pacman -Sy gzip
;; MS Windows: scoop install gzip
;;
;; curl for elfeed
;; ArchLinux: pacman -Sy curl
;; MS Windows: scoop install curl
;;
;; clojure for cider
;; ArchLinux: pacman -Sy clojure
;; MS Windows: scoop install leiningen
;;
;; mpv for pomodoro
;; ArchLinux: pacman -Sy mpv
;; MS Windows: scoop install mpv

;;; Commentary:
;;

;;; Code:

;; straight.el bootstrapping code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package - A configuration macro for simplifying your .emacs
;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; no-littering - help keeping ~/.emacs.d clean
;; https://github.com/emacscollective/no-littering
;; NOTE: Load this package as early as possible in your init file.
;; Make sure you load it at least before you change any path variables
;; using some other method
(use-package no-littering
  :custom
  (server-auth-dir (no-littering-expand-var-file-name "server/")))

;; Skip startup screen
(setq inhibit-startup-screen t)

;; Disable menu (yes, it's available in terminal emulators too)
(menu-bar-mode -1)

;; GUI settings residing in separate file
;; NOTE: no conditional checks, so GUI settings will be applied even
;; in server mode
(load (concat user-emacs-directory "gui"))

;; MS Windows specific settings
(if (string-equal system-type "windows-nt")
    (load (concat user-emacs-directory "mswindows")))

;; Settings with personal data which I prefer not to share in VCS
(load "~/essential/emacs/personal-prefs" t)

;; Geographical settings for calculating sunset/sunrise times which I
;; prefer not to share in VCS
(load "~/essential/emacs/regional-prefs" t)

;; save customization settings into its own file
(setq custom-file (concat user-emacs-directory "customizations.el"))
(load custom-file t)

;; UTF-8 language environment
(set-language-environment "UTF-8")

;; use xsel or wl-copy/wl-paste to copy/paste to and from clipboard in
;; Emacs running in terminal emulator under X11 or Wayland
(unless (display-graphic-p)
  (let ((session-type (getenv "XDG_SESSION_TYPE")))
    (cond
     ((string-equal session-type "wayland")
      (defun cut-function (text &optional push)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "wl-copy" nil 0 nil)))
      (defun paste-function()
        (let ((cmd-output (shell-command-to-string "wl-paste")))
          (unless (string= (car kill-ring) cmd-output)
            cmd-output))))
     ((string-equal session-type "x11")
      (defun cut-function (text &optional push)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max)
                               "xsel" nil 0 nil "-i" "-b")))
      (defun paste-function()
        (let ((cmd-output (shell-command-to-string "xsel -o -b")))
          (unless (string= (car kill-ring) cmd-output)
            cmd-output))))))
  (setq interprogram-cut-function 'cut-function)
  (setq interprogram-paste-function 'paste-function))

;; use hexadecimal codes to insert characters with C-q (quoted-insert)
(setq read-quoted-char-radix 16)

;; display column numbers in the mode line
(column-number-mode)

;; indent only with spaces, not tabs
(setq-default indent-tabs-mode nil)

;; use dimmed color to 'shadow' parts of filenames in the minibuffer
;; when running under terminal emulator
(setq file-name-shadow-tty-properties '(face file-name-shadow field shadow))

;; sort apropos matches by scores; best match is shown first
(setq apropos-sort-by-scores t)

;; automatically show help for active buffer text at point
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 2)
(help-at-pt-set-timer)

;; periodically save the list of bookmarks
(setq bookmark-save-flag 5)

;; do not recenter point in the window after moving it just a little
;; off the screen
(setq scroll-conservatively 1)

;; sort colors for 'list-colors-display' by hue, saturation, value
(setq list-colors-sort 'hsv)

;; count columns in the mode line starting from 1
(setq column-number-indicator-zero-based nil)

;; 24-hour format for time display in the mode line
(setq display-time-24hr-format t)

;; use visible bell instead of annoying beeps
(setq visible-bell t)

;; display raw bytes in hex format
(setq display-raw-bytes-as-hex t)

;; spellchecker settings
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "american")

;; make numbered backups
(setq version-control t)

;; delete excess backup files silently
(setq delete-old-versions t)

;; make backup files by copying the old file
(setq backup-by-copying t)

;; time stamp format
(setq time-stamp-start "Last Modified:[ 	]+\\\\?[\"<]+")

;; update the time stamp string(s) in the buffer every time you save the file
(add-hook 'before-save-hook 'time-stamp)

;; switches to pass to `ls' for verbose listing
(setq list-directory-verbose-switches "-hl")

;; By default auto saves appear in the current directory of a visited file
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; auto-save list of recent files every so often
(run-at-time t (* 15 60)
	     (lambda ()
	       (let ((inhibit-message t))
		 (recentf-save-list))))

;; input method activated automatically by C-\
(setq default-input-method "russian-computer")

;; calendar
(setq calendar-mark-diary-entries-flag t)

;; diary
(setq diary-file "~/essential/diary")
(setq diary-number-of-entries 5)

;; Switches passed to `ls' for Dired
;; MS Windows doesn't support ls option "--group-directories-first"
(unless (string-equal system-type "windows-nt")
    (setq dired-listing-switches "-al --group-directories-first"))

;; Isearch in Dired matches file names when initial point position is
;; on a file name
;; Otherwise, it searches the whole buffer
(setq dired-isearch-filenames 'dwim)
;; external image viewer in image-dired mode
(setq image-dired-external-viewer "feh")

;; When you visit a file, point goes to the last place where it was
;; when you previously visited the same file
(save-place-mode 1)

;; save the state of Emacs from one session to another
(desktop-save-mode 1)

;; save minibuffer history from one session to another
(savehist-mode 1)

;; show line numbers in all programming language modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; only use auto-fill inside comments
(setq comment-auto-fill-only-comments t)

;; automatically wrap long lines in comments
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; visually wrap long lines in text files
(add-hook 'text-mode-hook 'visual-line-mode)

;; show matching open-paren when close-paren is inserted
(setq blink-matching-paren 'jump-offscreen)

;; highlight matching parens
(show-paren-mode 1)

;; switches for M-x man command. Use M-n and M-p to switch between man
;; pages in different sections
(setq Man-switches "-a")

;; LSP Mode recommended performance settings
;; See https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; toggle visualization of many kinds of whitespace in the buffer
(global-set-key (kbd "H-$") 'whitespace-mode)
;; C-z minimizes window by default, it's annoying to accidentally
;; press it
(global-unset-key (kbd "C-z"))

;; 'what-cursor-position' will show the name of the character in
;; addition to the decimal/hex/octal representation
(setq what-cursor-show-names t)

;; prevents outdated byte code files from being loaded
(setq load-prefer-newer t)

;; * Packages: Built-in *

;; recentf - enable "Open Recent" menu for recent files
(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

;; tab-bar - faces customized to match Nova theme
(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-select-tab-modifiers '(hyper))
  (tab-bar-tab-hints t)
  :custom-face
  (tab-bar ((nil (:background "#303d44"))))
  (tab-bar-tab ((nil (:foreground "#7fc1ca" :background "#6a7d89"))))
  (tab-bar-tab-inactive ((nil (:foreground "#7fc1ca" :background "#303d44"))))
  :config
  (tab-bar-mode))

;; windmove - move between windows & window configurations
(use-package windmove
  :bind (("H-h" . windmove-left)
         ("H-j" . windmove-down)
         ("H-k" . windmove-up)
         ("H-l" . windmove-right)))

;; winner - a global minor mode that records the changes in the window
;; configuration, so that you can undo them
(use-package winner
  :bind (("H-," . winner-undo)
         ("H-." . winner-redo)))


;; * Packages: Look & feel *

;; nova-theme - A dark, pastel Emacs color theme
;; https://github.com/muirmanders/emacs-nova-theme/
(defun my/load-nova-theme (frame)
  (select-frame frame)
  (load-theme 'nova t))

(use-package nova-theme
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/load-nova-theme)
    (load-theme 'nova t)))

;; ace-window - Quickly switch windows in Emacs
;; https://github.com/abo-abo/ace-window/
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face ((t (:foreground "orange" :weight bold)))))

;; auto-compile - automatically compile Emacs Lisp libraries
;; https://github.com/emacscollective/auto-compile/
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; avy - Jump to arbitrary positions in visible text and select text quickly
;; https://github.com/abo-abo/avy/
(use-package avy
  :bind ("C-'" . avy-goto-char))

;; blackout - Better mode lighter overriding
;; https://github.com/raxod502/blackout
(use-package blackout
  :config
  (blackout 'auto-fill-function " AF")
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "ELisp"))

;; consult - Consulting completing-read
;; https://github.com/minad/consult
(use-package consult
  ;; TODO: consult-yank doesn't work first time in Windows 10
  ;; Should use interprogram-paste-function -> current-kill
  :bind (("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ;;("C-y" . consult-yank)
         ("M-y" . consult-yank-pop)))

;; consult-selectrum - Selectrum integration for Consult
;; https://github.com/minad/consult
(use-package consult-selectrum
  :after selectrum)

;; ctrlf - Emacs finally learns how to ctrl+F
;; https://github.com/raxod502/ctrlf
(use-package ctrlf
  :config
  (ctrlf-mode))

;; embark - Conveniently act on minibuffer completions
;; https://github.com/oantolin/embark/
(use-package embark
  :bind
  ("M-o" . embark-act))
  
;; NOTE: Has the development of Helm stopped?
;; See: https://www.reddit.com/r/emacs/comments/iqytf6/has_the_development_of_helm_stopped/
;; helm - Emacs incremental completion and selection narrowing framework
;; https://github.com/emacs-helm/helm
;; https://emacs-helm.github.io/helm/
;; (use-package helm
;;   :bind (("M-x" . helm-M-x)
;; 	 ("C-x b" . helm-buffers-list)
;; 	 ("C-x C-f" . helm-find-files)
;; 	 ("C-h a" . helm-apropos)
;; 	 ("M-y" . helm-show-kill-ring))
;;   :init
;;   (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
;;   (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))

;; hercules - An auto-magical, which-key-based hydra banisher
;; https://gitlab.com/jjzmajic/hercules.el/
(use-package hercules)

;; marginalia - Enrich existing commands with completion annotations
;; https://github.com/minad/marginalia/
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy nil marginalia-annotators-light))
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  :init
  (marginalia-mode))

;; Russian holidays for Emacs builtin calendar
;; https://github.com/grafov/russian-holidays
(use-package russian-holidays
  :config
  (setq holiday-other-holidays russian-holidays))

;; selectrum - Easily select item from list
;; https://github.com/raxod502/selectrum
(use-package selectrum
  :custom
  (selectrum-count-style 'current/matches)
  (selectrum-show-indices t)
  :config
  (selectrum-mode))

;; selectrum-prescient - Selectrum integration with prescient
;; https://github.com/raxod502/prescient.el
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

;; which-key - Emacs package that displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :blackout
  :custom
  (which-key-idle-delay 0.25)
  :config
  (which-key-mode))


;; * Packages: Office *

;; markdown-mode - Major mode for Markdown-formatted text
;; https://github.com/jrblevin/markdown-mode/
;; https://jblevins.org/projects/markdown-mode/
;; https://leanpub.com/markdown-mode/read
(use-package markdown-mode
  :mode "\\.md\\'")

;; org - Outline-based notes management and organizer
;; https://orgmode.org
(use-package org
  :bind
  ("H-o a" . org-agenda)
  ("H-o c" . org-capture)
  ("H-o l" . org-store-link)
  :hook
  (org-mode . (lambda () (electric-indent-local-mode -1)))
  :custom
  (org-directory "~/essential/orgmode/")
  (org-default-notes-file (concat org-directory "org-capture.org"))
  (org-capture-templates
   '(("t" "TODO" entry (file+headline "todo.org" "Tasks")
      "* TODO %?")
     ("w" "TV Show Watchlist" entry (file+headline "watchlist.org" "TV Shows")
      "* %^{Name} (%^{Year|2020})
Genre: %^{Genre|Drama|Crime|Comedy|Sci-Fi|Horror|Thriller|Documentary|Animation}
TV Network: %^{TV Network|Netflix|HBO|Amazon|Apple TV+|Hulu|Syfy}
Trailer: %^{Trailer URL}%?")
     ("W" "Movie Watchlist" entry (file+headline "watchlist.org" "Movies")
      "* %^{Name} (%^{Year|2020})
Genre: %^{Genre|Drama|Crime|Comedy|Sci-Fi|Horror|Thriller|Documentary|Animation}
Director: %^{Director}
IMDB URL: %^{IMDB URL}%?")))
  (org-agenda-include-diary t)
  (org-special-ctrl-a/e t))

;; typo - Emacs mode for typographical editing
;; https://github.com/jorgenschaefer/typoel
(use-package typo
  :custom
  (typo-language "Russian"))


;; * Packages: Multimedia *

;; elfeed - an Emacs Atom/RSS feed reader
;; https://github.com/skeeto/elfeed/
(use-package elfeed
  :config
  (defun my/eww-browse-elfeed-entry ()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
          (when link
            (eww-browse-url link))))
  :bind (:map elfeed-show-mode-map
              ("e" . my/eww-browse-elfeed-entry)))

;; elfeed-org - Configure elfeed with one or more org-mode files
;; https://github.com/remyhonig/elfeed-org/
(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (list (concat org-directory "elfeed.org")))
  :config
  (elfeed-org))

;; * Packages: Programming (general) *

;; company - Modular text completion framework
;; https://github.com/company-mode/company-mode/
(use-package company
  :blackout)

;; dap-mode - Debug Adapter Protocol mode
;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode)

;; dumb-jump - jump to definition for 40+ languages without
;; configuration
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :bind (("C-c [" . dumb-jump-back)
         ("C-c ]" . dumb-jump-go))
  :config
  (setq dumb-jump-prefer-searcher 'rg))

;; editorconfig - EditorConfig plugin for emacs
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :blackout
  :config
  (editorconfig-mode 1))

;; flycheck - On the fly syntax checking for GNU Emacs
;; https://github.com/flycheck/flycheck
;; http://www.flycheck.org
(use-package flycheck
  :bind (:map flycheck-mode-map
              ("H-f [" . flycheck-previous-error)
              ("H-f ]" . flycheck-next-error)
              ("H-f l" . flycheck-list-errors))
  :config
  (global-flycheck-mode))

;; NOTE: Has the development of Helm stopped?
;; See: https://www.reddit.com/r/emacs/comments/iqytf6/has_the_development_of_helm_stopped/
;; helm-projectile - Helm integration for Projectile
;; https://github.com/bbatsov/helm-projectile/
;; (use-package helm-projectile
;;   :config (helm-projectile-toggle 1))

;; NOTE: Has the development of Helm stopped?
;; See: https://www.reddit.com/r/emacs/comments/iqytf6/has_the_development_of_helm_stopped/
;; helm-rg - a helm interface to ripgrep
;; https://github.com/cosmicexplorer/helm-rg/
;; (use-package helm-rg)

;; rg - A search tool based on ripgrep
;; https://github.com/dajva/rg.el/
;; https://rgel.readthedocs.io/
(use-package rg)

;; lsp-mode - Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :after which-key
  :init
  (setq lsp-keymap-prefix "H-c")
  :hook ((c++-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (mhtml-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-disabled-clients '(mhtml-mode . angular-ls)))


;; lsp-ui - flycheck integration and higher level UI modules for lsp-mode
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; Magit - A Git Porcelain inside Emacs
;; https://github.com/magit/magit
;; https://magit.vc
(use-package magit
  :defer 2)

;; pomodoro - A timer for the Pomodoro Technique
;; https://github.com/baudtack/pomodoro.el
(use-package pomodoro
  :commands pomodoro-start
  :custom
  (pomodoro-break-start-sound
   (concat user-emacs-directory (file-name-as-directory "sounds") "bells.mp3"))
  (pomodoro-work-start-sound
   (concat user-emacs-directory (file-name-as-directory "sounds") "b15.mp3"))
  (pomodoro-break-start-message "pomodoro: Break time!")
  (pomodoro-work-start-message "pomodoro: Back to work!")
  (pomodoro-long-break-start-message "pomodoro: Time for a longer break!")
  (pomodoro-sound-player "mpv")
  (pomodoro-work-cycle "🍅")
  (pomodoro-break-cycle "☕")
  (pomodoro-desktop-notification t)
  :init
  (defvar my/pomodoro-added-to-modeline-p nil
    "Non-nil if pomodoro widget was already added to the mode line")
  :config
  (unless my/pomodoro-added-to-modeline-p
    (pomodoro-add-to-mode-line)
    (setq my/pomodoro-added-to-modeline-p t)))

;; Projectile - Manage and navigate projects in Emacs easily
;; https://github.com/bbatsov/projectile/
;; https://docs.projectile.mx/
(use-package projectile
  :bind-keymap
  ("H-p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/projects/"))
  (projectile-mode-line-prefix " Prj")
  :config
  (projectile-mode +1))

;; tree-sitter - Incremental parsing system
;; https://github.com/ubolonton/emacs-tree-sitter/
;; https://ubolonton.github.io/emacs-tree-sitter/
;; https://tree-sitter.github.io/tree-sitter/
(use-package tree-sitter
  :blackout " 🌳")

;; tree-sitter-langs - Grammar bundle for tree-sitter
(use-package tree-sitter-langs)

;; vterm - a fully-fledged terminal emulator inside GNU Emacs based on libvterm
;; https://github.com/akermu/emacs-libvterm/
(when (eq system-type 'gnu/linux)
  (use-package vterm
    :custom
    (vterm-buffer-name-string "λ %s")))

;; yasnippet - Yet another snippet extension for Emacs
;; https://github.com/joaotavora/yasnippet/
;; http://joaotavora.github.io/yasnippet/
(use-package yasnippet)

;; zeal-at-point - Search the word at point with Zeal
;; https://github.com/jinzhu/zeal-at-point/
;; NOTE: currently doesn't work in MS Windows
;; See https://github.com/zealdocs/zeal/issues/1113
(use-package zeal-at-point
  :bind ("\C-cd" . zeal-at-point))

;; * Packages: Programming (Arch Linux) *

;; pkgbuild-mode - Interface to the ArchLinux package manager
;; https://github.com/juergenhoetzel/pkgbuild-mode/
(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

;; * Packages: Programming (Frontend web development) *

;; emmet-mode - Unofficial Emmet's support for Emacs
;; https://github.com/smihica/emmet-mode/
(use-package emmet-mode
  :hook sgml-mode)

;; js-doc - Insert JsDoc style comment easily
;; https://github.com/mooz/js-doc/
(use-package js-doc
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c i" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag)))

;; js2-mode - Improved JavaScript editing mode for GNU Emacs
;; https://github.com/mooz/js2-mode/
(use-package js2-mode
  :mode "\\.js\\'")

;; json-mode - Major mode for editing JSON files
;; https://github.com/joshwnj/json-mode/
(use-package json-mode
  :mode "\\.json\\'")

;; yaml-mode - Major mode for editing YAML files
;; https://github.com/yoshiki/yaml-mode/
(use-package yaml-mode
  :mode "\\.yaml\\'")

;; * Packages: Programming (Clojure) *

;; cider - Clojure Interactive Development Environment that Rocks
;; https://github.com/clojure-emacs/cider/
;; https://docs.cider.mx/
(use-package cider)

;; paredit - minor mode for editing parentheses
(use-package paredit
  :hook (clojure-mode . paredit-mode))

;; rainbow-delimiters - Highlight brackets according to their depth
;; https://github.com/Fanael/rainbow-delimiters/
(use-package rainbow-delimiters
  :hook ((clojure-mode
          cider-repl-mode
          emacs-lisp-mode
          lisp-interaction-mode) . rainbow-delimiters-mode))

;; * Packages: Programming (Rust) *

;; rust-mode - Emacs configuration for Rust
;; https://github.com/rust-lang/rust-mode
;; rustfmt: Tool to find and fix Rust formatting issues
;; https://github.com/rust-lang-nursery/rustfmt
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))


;; enable commands which can confuse novices
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'init)

;;; init.el ends here
