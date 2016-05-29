module PathSearch

export run_search, Node, Path, Segment, Graph, BothWayGraph, Frontier

type Node
	name::AbstractString
end

type Segment
	from::Node
	to::Node
	length::Int
end

function switcheroo(seg::Segment)
	Segment(seg.to, seg.from, seg.length)
end

type Path
	segments::Array{Segment, 1}
end

function last_segment(p::Path)
	p.segments[end]
end

function add_segment(p::Path, s::Segment)
	Path(vcat(p.segments, [s]))
end

type Frontier
	paths::Array{Path, 1}
end

function add_paths(f::Frontier, ps::Array{Path, 1})
	Frontier(vcat(f.paths, ps))
end

abstract Graph

type BothWayGraph <: Graph
	one_way_edges::Array{Segment, 1}
end

function edges(g::BothWayGraph)
	inverted = map(switcheroo, g.one_way_edges)
	vcat(g.one_way_edges, inverted)
end

type OneWayGraph <: Graph
	one_way_edges::Array{Segment, 1}
end

function edges(g::OneWayGraph)
	g.one_way_edges
end

# node A , node B , graph of segents  ->  (array) of best paths
function outgoing_segments(n::Node, g::Graph)
	filter(x -> x.from.name == n.name ,  edges(g))
end

function extend_frontier(p::Path, f::Frontier, g::Graph)
	osegs = outgoing_segments(last_segment(p).to,g)
	paths = map(x -> add_segment(p,x) , osegs)
	add_paths(f,paths)
end



function take_choice(f::Frontier)
	first_path = f.paths[1]
	remainder = Frontier(f.paths[2:length(f.paths)])
	(first_path, remainder)
end

function path_search(f::Frontier, g::Graph, e::Node)
	(first_path, remainder) = take_choice(f)
	if last_segment(first_path).to.name == e.name
		first_path
	else
		path_search(extend_frontier(first_path, remainder, g), g, e)
	end
end

function run_search(start::Node, end_node::Node, g::Graph)

	init_frontier = Frontier([Path([Segment(start, start, 0)])])
	first_best_path = path_search(init_frontier, g, end_node)
end


end #PathSearch module

