;; hunspell settings
(setq ispell-hunspell-dictionary-alist '(
				      (nil
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US")
				       nil
				       iso-8859-1)

				      ("american"
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US")
				       nil
				       iso-8859-1)
				      
				      ("russian"
				       "\\cy"
				       "\\Cy"
				       "[-]"
				       nil
				       ("-d" "ru_RU")
				       nil
				       utf-8)
				      ))
