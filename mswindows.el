;; hunspell workaround for Windows 10
;; See: https://github.com/hunspell/hunspell/issues/669
(setq ispell-hunspell-dictionary-alist '(
				      (nil
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US")
				       nil
				       utf-8)

				      ("american"
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US")
				       nil
				       utf-8)
				      
				      ("russian"
				       "\\cy"
				       "\\Cy"
				       "[-]"
				       nil
				       ("-d" "ru_RU")
				       nil
				       utf-8)
				      ))

;; use Menu key on MS Windows keyboards as Hyper modifier
(setq w32-apps-modifier 'hyper)

;; Show directories before files in Dired
(setq ls-lisp-dirs-first t)

(setq dired-listing-switches "-ahl")

;; Coding system used for system messages, for decoding keyboard input
;; on X Window system, and for encoding standard output and error
;; streams
;; On MS Windows it is set to 'cp1252 by default and output from
;; functions like 'format-time-string' uses wrong character glyphs
(setq locale-coding-system 'cp1251)
