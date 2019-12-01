;; Disable toolbar and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set nice default font
;; ArchLinux: pacman -Sy ttf-hack
;; MS Windows: choco install hackfont
(set-face-attribute 'default nil :family "Hack" :height 110)
;; display cyrillic characters encoded with CP1251 using default Unicode font
;; see https://lists.gnu.org/archive/html/bug-gnu-emacs/2009-11/msg00165.html
(set-fontset-font t 'cyrillic (font-spec :registry "iso10646-1" :script 'cyrillic))
;; TODO: fixed-pitch-serif - fixed-with font with serifs
;; TODO: variable-pitch - variable-width font

;; use an icon as mail indicator in the mode line on a graphic display
(setq display-time-use-mail-icon t)

;; mode line display for end-of-line format
(setq eol-mnemonic-dos "(Win)")

;; blink cursor more slowly
(setq blink-cursor-interval 0.7)

;; more smooth text scaling
(setq text-scale-mode-step 1.1)
