;; Disable toolbar and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Don't use modal GUI dialog boxes as prompts
(setq use-dialog-box nil)

(cond
 ((string-equal system-name "LAPTOP-FPI35DV6")
  (setq my/font-height 100))
 ((string-equal system-name "DESKTOP")
  (setq my/font-height 110)))

;; Set nice default font
;; ArchLinux: pacman -Sy ttf-hack
;; MS Windows: choco install hackfont
(set-face-attribute 'default nil :family "Hack" :height my/font-height)
;; display cyrillic characters encoded with CP1251 using default Unicode font
;; see https://lists.gnu.org/archive/html/bug-gnu-emacs/2009-11/msg00165.html
(set-fontset-font t 'cyrillic (font-spec :registry "iso10646-1"
                                         :script 'cyrillic))
;; fixed-pitch used in lsp-ui-doc popups (ugly default Monospace font
;; in MS Windows)
;; ArchLinux: pacman -Sy ttf-fira-code
;; MS Windows: choco install firacode
(set-face-attribute 'fixed-pitch nil :family "Fira Code" :height my/font-height)
;; ArchLinux: http://dimkanovikov.pro/courierprime/
;; MS Windows: http://dimkanovikov.pro/courierprime/
(set-face-attribute 'fixed-pitch-serif nil :family "Courier Prime" :height my/font-height)
;; TODO: variable-pitch - variable-width font

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; use an icon as mail indicator in the mode line on a graphic display
(setq display-time-use-mail-icon t)

;; mode line display for end-of-line format
(setq eol-mnemonic-dos "(Win)")

;; blink cursor more slowly
(setq blink-cursor-interval 0.7)

;; more smooth text scaling
(setq text-scale-mode-step 1.1)

;; more slow and smooth touchpad scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
