(require easy
	 treesearch
         test)

(def. (number.haskell-string s)
  (number->string s))

(def. (string.haskell-string s)
  (object->string s))

(def. (symbol.haskell-string s)
  (string.haskell-string (symbol.string s)))

(def. (citylink.haskell-string l)
  (let. ((from to distance) l)
	(string-append "(Segment (Node "
		       (.haskell-string from)
		       ") (Node "
		       (.haskell-string to)
		       ") "
		       (.haskell-string distance)
		       ")")))

(def (links->haskell links)
     (string-append "a = ["
		    (strings-join (map .haskell-string links)
				  ",\n     ")
		    "]"))

