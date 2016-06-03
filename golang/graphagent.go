package main

import (
	"fmt"
)

type Segment struct {
	from Node
	to Node
	length int
}

type Graph struct {
	edges []Segment
}

type Node struct {
	name string
}

type Frontier struct {
	paths []Path
}

type Path struct {
	steps []Segment
}


func main() {

	graph := Graph{[]Segment{Segment{Node{"Arad"}, Node{"Zerind"}, 75},
     Segment{Node{"Arad"}, Node{"Sibiu"}, 140},
     Segment{Node{"Arad"}, Node{"Timisoara"}, 118},
     Segment{Node{"Zerind"}, Node{"Dradea"}, 71},
     Segment{Node{"Dradea"}, Node{"Sibiu"}, 151},
     Segment{Node{"Sibiu"}, Node{"Fagaras"}, 99},
     Segment{Node{"Sibiu"}, Node{"RimnicuVilcea"}, 80},
     Segment{Node{"Fagaras"}, Node{"Bucharest"}, 211},
     Segment{Node{"RimnicuVilcea"}, Node{"Pitesti"}, 97},
     Segment{Node{"RimnicuVilcea"}, Node{"Craiova"}, 146},
     Segment{Node{"Pitesti"}, Node{"Craiova"}, 138},
     Segment{Node{"Pitesti"}, Node{"Bucharest"}, 101},
     Segment{Node{"Timisoara"}, Node{"Lugoj"}, 111},
     Segment{Node{"Lugoj"}, Node{"Mehadia"}, 70},
     Segment{Node{"Mehadia"}, Node{"Drobeta"}, 75},
     Segment{Node{"Drobeta"}, Node{"Craiova"}, 120},
     Segment{Node{"Bucharest"}, Node{"Giurgiu"}, 90},
     Segment{Node{"Bucharest"}, Node{"Urziceni"}, 85},
     Segment{Node{"Urziceni"}, Node{"Hirsova"}, 98},
     Segment{Node{"Hirsova"}, Node{"Eforie"}, 86},
     Segment{Node{"Urziceni"}, Node{"Vaslui"}, 142},
     Segment{Node{"Vaslui"}, Node{"Iasi"}, 92},
     Segment{Node{"Iasi"}, Node{"Neamt"}, 87}}}

	graphb := graph.makebothways()
	//fmt.Println(graph5)

	output := outgoing_segments(Node{"Arad"}, graphb)
	for i := 0; i < len(output); i++ {
		fmt.Println(output[i])
		fmt.Println("**************")
	}


	testpath := Path{[]Segment{Segment{Node{"Arad"}, Node{"Zerind"}, 75}, Segment{Node{"Zerind"}, Node{"Dradea"}, 71} }}
	testfront := Frontier{[]Path{testpath}}

	extendoutput := extend_frontier(testpath, testfront, graphb)

	for i := 0; i < len(extendoutput.paths); i++ {
		fmt.Println(extendoutput.paths[i])
	}

	testpath2 := Path{[]Segment{Segment{Node{"Arad"}, Node{"Zerind"}, 75}, Segment{Node{"Zerind"}, Node{"Dradea"}, 71} }}
	testfront2 := Frontier{[]Path{testpath2}}

	pathsearchoutputtest := path_search(testfront2, graphb, Node{"Timisoara"})
	fmt.Println("***************")
	fmt.Println(pathsearchoutputtest)


	fmt.Println("************")
	fmt.Println("************")
	runsearchoutput := runsearch(Node{"Arad"}, Node{"Dradea"}, graphb)
	fmt.Println(runsearchoutput)

}

func (s *Segment) switcheroo () Segment {

	seg2 := Segment{s.to, s.from, s.length}
	return seg2
}

func (g *Graph) makebothways () Graph {

	onewaylength := len(g.edges)
	for i := 0; i < onewaylength; i++ {
		g.edges = append(g.edges, g.edges[i].switcheroo())
	}

	return Graph{g.edges}
}


func outgoing_segments (n Node, g Graph) []Segment {
	var osegs []Segment
	for i := 0; i < len(g.edges); i++ {
		if g.edges[i].from.name == n.name {
			osegs = append(osegs, g.edges[i])
		}
	}

	return osegs
}

func extend_frontier (p Path, f Frontier, g Graph) Frontier {
	osegs := outgoing_segments(p.steps[len(p.steps) -1].to, g)
	var newpaths []Path
	for i := 0; i < len(osegs); i++ {
		newpaths = append(newpaths, Path{append(p.steps, osegs[i])})
	}

	for j := 0; j < len(newpaths); j++ {
		f.paths = append(f.paths, newpaths[j])
	}

	return f
}

func path_search(f Frontier, g Graph, e Node) Path {
	nextpath, remainder := f.paths[0], f.paths[1:]
	if nextpath.steps[ len(nextpath.steps) -1 ].to.name == e.name {
		return nextpath
	} else {
		return path_search(extend_frontier(nextpath, Frontier{remainder}, g), g, e)
	}

}

func runsearch(start_node Node, end_node Node, g Graph) Path {
		init_frontier := Frontier{[]Path{Path{[]Segment{Segment{Node{start_node.name}, Node{start_node.name}, 0}}}}}


		first_best_path := path_search(init_frontier, g, end_node)

		return first_best_path
}
