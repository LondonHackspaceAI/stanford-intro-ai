(require easy
	 ai)

(def. (number.julia-string s)
  (number->string s))

(def. (string.julia-string s)
  (object->string s))

(def. (symbol.julia-string s)
  (string.julia-string (symbol.string s)))

(def. (citylink.julia-string l)
  (let. ((from to distance) l)
	(string-append "citylink("
		       (.julia-string from)
		       ", "
		       (.julia-string to)
		       ", "
		       (.julia-string distance)
		       ")")))

(def (links->julia)
     (string-append "a = ["
		    (strings-join (map .julia-string l1)
				  ",\n     ")
		    "];"))

