;;; init.el --- Emacs personal startup file
;;
;; External dependencies of my Emacs configuration
;;
;; diff for M-x diff and related commands
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
;; LSP servers for CSS, HTML, JavaScript, TypeScript
;; ArchLinux & MS Windows: npm install -g vscode-css-languageserver-bi vscode-html-languageserver-bin typescript-language-server typescript
;;
;; LSP server for Rust
;; ArchLinux & MS Windows: rustup component add rls
;;
;; markdown processor for markdown-mode (used for rendering HTML for preview and export)
;; ArchLinux: pacman -Sy discount
;; MS Windows: ?
;;
;; Ripgrep for helm, helm-rg
;; ArchLinux: pacman -Sy ripgrep
;; MS Windows: scoop install ripgrep
;;
;; rustfmt for rust-mode
;; ArchLinux & MS Windows: rustup component add rustfmt

;;; Commentary:
;; 

;;; Code:

;; Skip startup screen
(setq inhibit-startup-screen t)

;; Disable menu (yes, it's available in terminal emulators too)
(menu-bar-mode -1)

;; GUI settings residing in separate file
(if (display-graphic-p)
    (load "~/.emacs.d/gui"))

;; MS Windows specific settings
(if (string-equal system-type "windows-nt")
    (load "~/.emacs.d/mswindows"))

;; Geographical settings for calculating sunset/sunrise times which I
;; prefer not to share in VCS
(load "~/.emacs.d/regional-prefs" t)

;; save customization settings into its own file
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file t)

;; use xsel to copy/paste in Emacs running in terminal emulator
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "-i" "-b")))
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel -o -b")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ))

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

;; do not recenter point in the window after moving it just a little off the screen
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

;; store all backup files in one dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

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

;; human-readable info about the amount of free space for C-u C-x C-d
(setq directory-free-space-args "-hP")

;; enable "Open Recent" menu for recent files
(recentf-mode)

;; auto-save list of recent files every 5 minutes
(run-at-time t (* 5 60)
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
(setq dired-listing-switches "-al --group-directories-first")
;; Isearch in Dired matches file names when initial point position is on a file name.
;; Otherwise, it searches the whole buffer
(setq dired-isearch-filenames 'dwim)
;; external image viewer in image-dired mode
(setq image-dired-external-viewer "feh")

;; When you visit a file, point goes to the last place where it was when you previously
;; visited the same file
(save-place-mode 1)

;; save the state of Emacs from one session to another
(desktop-save-mode 1)

;; save minibuffer history from one session to another
(savehist-mode 1)

;; show line numbers in all programming language modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; automatically wrap long lines in comments
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; show matching open-paren when close-paren is inserted
(setq blink-matching-paren 'jump-offscreen)

;; highlight matching parens
(show-paren-mode 1)

;; switches for M-x man command. Use M-n and M-p to switch between man pages in different sections
(setq Man-switches "-a")

;; helps with LSP frequent freezes/stuttering
;; See https://emacs-lsp.github.io/lsp-mode/lsp-mode.html#_frequent_freezesstuttering
(if (< emacs-major-version 27)
    (setq gc-cons-threshold 30000000))

;; global key bindings
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; prevents outdated byte code files from being loaded
(setq load-prefer-newer t)

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

;; * Packages: Look & feel *

;; nova-theme - A dark, pastel Emacs color theme
;; https://github.com/muirmanders/emacs-nova-theme/
(use-package nova-theme)

;; ace-window - Quickly switch windows in Emacs
;; https://github.com/abo-abo/ace-window/
(use-package ace-window
  :bind ("C-x o" . ace-window))

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

;; delight - A dimmer switch for your lighter text (remove or change
;; major & minor mode strings in your mode-line)
;; https://elpa.gnu.org/packages/delight.html
(use-package delight)

;; helm - Emacs incremental completion and selection narrowing framework
;; https://github.com/emacs-helm/helm
;; https://emacs-helm.github.io/helm/
(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ("C-h a" . helm-apropos)
	 ("M-y" . helm-show-kill-ring))
  :init
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))

;; hercules - An auto-magical, which-key-based hydra banisher
;; https://gitlab.com/jjzmajic/hercules.el/
(use-package hercules)

;; Russian holidays for Emacs builtin calendar
;; https://github.com/grafov/russian-holidays
(use-package russian-holidays
  :config
  (require 'holidays)
  (setq calendar-holidays (append calendar-holidays russian-holidays)))

;; which-key - Emacs package that displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (which-key-mode))


;; * Packages: Office *

;; mardkown-mode - Major mode for Markdown-formatted text
;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode)

;; Outline-based notes management and organizer
;; https://orgmode.org
(use-package org)

;; typo - Emacs mode for typographical editing
;; https://github.com/jorgenschaefer/typoel
(use-package typo
  :config (setq-default typo-language "Russian"))


;; * Packages: Programming (general) *

;; company - Modular text completion framework
;; https://github.com/company-mode/company-mode/
(use-package company)

;; company-lsp - Company completion backend for lsp-mode
;; Company completion backend for lsp-mode
(use-package company-lsp
  :commands company-lsp)

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
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-selector 'helm))

;; editorconfig - EditorConfig plugin for emacs
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config (editorconfig-mode 1)
  :delight (editorconfig-mode " EC"))

;; flycheck - On the fly syntax checking for GNU Emacs
;; https://github.com/flycheck/flycheck
;; http://www.flycheck.org
(use-package flycheck
  :config (global-flycheck-mode))

;; helm-projectile - Helm integration for Projectile
;; https://github.com/bbatsov/helm-projectile/
(use-package helm-projectile
  :config (helm-projectile-toggle 1))

;; helm-rg - a helm interface to ripgrep
;; https://github.com/cosmicexplorer/helm-rg/
(use-package helm-rg)

;; lsp-mode - Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/lsp-mode.html
(use-package lsp-mode
  :hook ((css-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (mhtml-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :config
  (setq lsp-prefer-flymake nil)
  (add-to-list 'lsp-disabled-clients '(mhtml-mode . angular-ls))
  :commands lsp)

;; lsp-ui - flycheck integration and higher level UI modules for lsp-mode
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; Magit - A Git Porcelain inside Emacs
;; https://github.com/magit/magit
;; https://magit.vc
(use-package magit
  :defer 2)

;; Projectile - Manage and navigate projects in Emacs easily
;; https://github.com/bbatsov/projectile/
;; https://www.projectile.mx/en/latest/
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

;; yasnippet - Yet another snippet extension for Emacs
;; https://github.com/joaotavora/yasnippet/
;; http://joaotavora.github.io/yasnippet/
(use-package yasnippet)

;; zeal-at-point - Search the word at point with Zeal
;; https://github.com/jinzhu/zeal-at-point/
(use-package zeal-at-point
  :bind ("\C-cd" . zeal-at-point))

;; * Packages: Programming (Frontend web development) *

;; emmet-mode - Unofficial Emmet's support for Emacs
;; https://github.com/smihica/emmet-mode/
(use-package emmet-mode
  :config (add-hook 'sgml-mode-hook 'emmet-mode))

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
(use-package json-mode)

;; * Packages: Programming (Rust) *

;; rust-mode - Emacs configuration for Rust
;; https://github.com/rust-lang/rust-mode
;; rustfmt: Tool to find and fix Rust formatting issues
;; https://github.com/rust-lang-nursery/rustfmt
(use-package rust-mode
  :mode "\\.rs\\'"
  :config (setq rust-format-on-save t))

(provide 'init)

;;; init.el ends here
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
