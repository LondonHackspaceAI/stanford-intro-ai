(require easy
	 treesearch
	 test)

;; to save typing effort, enter values as a list of bare lists, then
;; convert them to citylink objects
(def links
     (lists->Segments
      '((Arad Zerind 75)
	(Arad Sibiu 140)
	(Arad Timisoara 118)
	(Zerind Dradea 71)
	(Dradea Sibiu 151)
	(Sibiu Fagaras 99)
	(Sibiu RimnicuVilcea 80)
	(Fagaras Bucharest 211)
	(RimnicuVilcea Pitesti 97)
	(RimnicuVilcea Craiova 146)
	(Pitesti Craiova 138)
	(Pitesti Bucharest 101)
	(Timisoara Lugoj 111)
	(Lugoj Mehadia 70)
	(Mehadia Drobeta 75)
	(Drobeta Craiova 120)
	(Bucharest Giurgiu 90)
	(Bucharest Urziceni 85)
	(Urziceni Hirsova 98)
	(Hirsova Eforie 86)
	(Urziceni Vaslui 142)
	(Vaslui Iasi 92)
	(Iasi Neamt 87))))


(TEST
 > (treesearch* links 'Arad 'Bucharest)
 (418 (Arad Sibiu RimnicuVilcea Pitesti Bucharest))
 > (treesearch* links 'Bucharest 'Arad)
 (418 (Bucharest Pitesti RimnicuVilcea Sibiu Arad)))

