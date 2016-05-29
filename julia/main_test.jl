

#main test

include("pathsearch.jl")

using PathSearch

graph = BothWayGraph([Segment(Node("Arad"), Node("Zerind"), 75),
     Segment(Node("Arad"), Node("Sibiu"), 140),
     Segment(Node("Arad"), Node("Timisoara"), 118),
     Segment(Node("Zerind"), Node("Dradea"), 71),
     Segment(Node("Dradea"), Node("Sibiu"), 151),
     Segment(Node("Sibiu"), Node("Fagaras"), 99),
     Segment(Node("Sibiu"), Node("RimnicuVilcea"), 80),
     Segment(Node("Fagaras"), Node("Bucharest"), 211),
     Segment(Node("RimnicuVilcea"), Node("Pitesti"), 97),
     Segment(Node("RimnicuVilcea"), Node("Craiova"), 146),
     Segment(Node("Pitesti"), Node("Craiova"), 138),
     Segment(Node("Pitesti"), Node("Bucharest"), 101),
     Segment(Node("Timisoara"), Node("Lugoj"), 111),
     Segment(Node("Lugoj"), Node("Mehadia"), 70),
     Segment(Node("Mehadia"), Node("Drobeta"), 75),
     Segment(Node("Drobeta"), Node("Craiova"), 120),
     Segment(Node("Bucharest"), Node("Giurgiu"), 90),
     Segment(Node("Bucharest"), Node("Urziceni"), 85),
     Segment(Node("Urziceni"), Node("Hirsova"), 98),
     Segment(Node("Hirsova"), Node("Eforie"), 86),
     Segment(Node("Urziceni"), Node("Vaslui"), 142),
     Segment(Node("Vaslui"), Node("Iasi"), 92),
     Segment(Node("Iasi"), Node("Neamt"), 87)])

println(PathSearch.run_search(Node("Arad"), Node("Bucharest"), graph))

