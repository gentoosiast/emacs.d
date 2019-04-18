;; External dependencies of my Emacs configuration
;;
;; EditorConfig C Core for editorconfig
;; ArchLinux: pacman -Sy editorconfig-core-c
;; MS Windows: scoop install editorconfig
;;
;; Git for straight.el package manager, magit
;; ArchLinux: pacman -Sy git
;; MS Windows: scoop install git
;;
;; Ripgrep for helm, helm-ag
;; ArchLinux: pacman -Sy ripgrep
;; MS Windows: scoop install ripgrep
;;
;; rustfmt for rust-mode
;; ArchLinux & MS Windows: rustup component add rustfmt

;; Skip startup screen
(setq inhibit-startup-screen t)

;; Disable menu (yes, it's available in terminal emulators too)
(menu-bar-mode -1)

;; GUI settings residing in separate file
(if (display-graphic-p)
    (load "~/.emacs.d/gui"))

;; Geographical settings for calculating sunset/sunrise times which I
;; prefer not to share in VCS
(load "~/.emacs.d/my-geo-coords" t)

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

;; A configuration macro for simplifying your .emacs
;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; * Packages: Look & feel *

;; A lab color space zenburn theme
;; https://github.com/ksjogo/labburn-theme
;(use-package labburn-theme)

;; nova-theme - A dark, pastel Emacs color theme
;; https://github.com/muirmanders/emacs-nova-theme/
(use-package nova-theme)

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
	 ("C-x C-f" . helm-find-files))
  :init
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))
					     

;; which-key - Emacs package that displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (which-key-mode))


;; * Packages: Office *

;; Outline-based notes management and organizer
;; https://orgmode.org
(use-package org)


;; * Packages: Programming (general) *

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

;; helm-ag - the silver searcher with helm interface (with support for ripgrep)
;; https://github.com/syohex/emacs-helm-ag/
(use-package helm-ag
  :init (setq helm-ag-base-command "rg --no-heading"))

;; lsp-mode - Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :config (add-hook 'js-mode-hook #'lsp))

;; Magit - A Git Porcelain inside Emacs
;; https://github.com/magit/magit
;; https://magit.vc
(use-package magit
  :defer 2)


;; * Packages: Programming (Rust) *

;; rust-mode - Emacs configuration for Rust
;; https://github.com/rust-lang/rust-mode
;; rustfmt: Tool to find and fix Rust formatting issues
;; https://github.com/rust-lang-nursery/rustfmt
(use-package rust-mode
  :mode "\\.rs\\'"
  :config (setq rust-format-on-save t))
