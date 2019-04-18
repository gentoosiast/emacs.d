;; Disable toolbar and scroll bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set nice default font
;; ArchLinux: pacman -Sy ttf-hack
;; MS Windows: choco install hackfont
(set-face-attribute 'default nil :family "Hack" :height 110)
