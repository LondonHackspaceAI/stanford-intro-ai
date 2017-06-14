-module(stanfordintro).
-export([main/0, switcheroo/1, make_both_ways/2, show_graph/1, extend_frontier/3]).

main() -> 
	Graph = [
	 { segment, { node,  "Arad"},  { node,  "Zerind"}, 75},
     { segment, { node,  "Arad"},  { node,  "Sibiu"}, 140},
     { segment, { node,  "Arad"},  { node,  "Timisoara"}, 118},
     { segment, { node,  "Zerind"},  { node,  "Dradea"}, 71},
     { segment, { node,  "Dradea"},  { node,  "Sibiu"}, 151},
     { segment, { node,  "Sibiu"},  { node,  "Fagaras"}, 99},
     { segment, { node,  "Sibiu"},  { node,  "RimnicuVilcea"}, 80},
     { segment, { node,  "Fagaras"},  { node,  "Bucharest"}, 211},
     { segment, { node,  "RimnicuVilcea"},  { node,  "Pitesti"}, 97},
     { segment, { node,  "RimnicuVilcea"},  { node,  "Craiova"}, 146},
     { segment, { node,  "Pitesti"},  { node,  "Craiova"}, 138},
     { segment, { node,  "Pitesti"},  { node,  "Bucharest"}, 101},
     { segment, { node,  "Timisoara"},  { node,  "Lugoj"}, 111},
     { segment, { node,  "Lugoj"},  { node,  "Mehadia"}, 70},
     { segment, { node,  "Mehadia"},  { node,  "Drobeta"}, 75},
     { segment, { node,  "Drobeta"},  { node,  "Craiova"}, 120},
     { segment, { node,  "Bucharest"},  { node,  "Giurgiu"}, 90},
     { segment, { node,  "Bucharest"},  { node,  "Urziceni"}, 85},
     { segment, { node,  "Urziceni"},  { node,  "Hirsova"}, 98},
     { segment, { node,  "Hirsova"},  { node,  "Eforie"}, 86},
     { segment, { node,  "Urziceni"},  { node,  "Vaslui"}, 142},
     { segment, { node,  "Vaslui"},  { node,  "Iasi"}, 92},
     { segment, { node,  "Iasi"},  { node,  "Neamt"}, 87}],

     Graph3 = make_both_ways([],Graph),

     % return if Start = Dest
     runsearch({node, "Arad"}, {node,"Pitesti"}, Graph3).



switcheroo({ segment, { node, NodeName1}, { node, NodeName2 }, Dist }) ->
	{ segment, { node, NodeName2}, { node, NodeName1 }, Dist }.

make_both_ways(Out, []) ->
	Out;
make_both_ways(Out, [H|SegList]) ->
	make_both_ways( lists:append( lists:append(Out, [H]), [switcheroo(H)] ), SegList).

show_graph([]) -> {ok};
show_graph([H|T]) ->
	io:fwrite("~p~n", [H]),
	show_graph(T).


outgoing_paths(SourceNode,Graph) ->
	lists:filter(fun(X) -> {segment, SourceNodeCheck, {node, _}, _} = X, SourceNode == SourceNodeCheck end, Graph).


build_extended_paths([], OrigPath, ExtendedPaths) -> ExtendedPaths;
build_extended_paths([H|NextStepsTail], OrigPath, ExtendedPaths) ->

	case ExtendedPaths of [[]] ->
		build_extended_paths(NextStepsTail, OrigPath, [lists:append(OrigPath,[H])]);
	 		_Else ->
		build_extended_paths(NextStepsTail, OrigPath, lists:append(ExtendedPaths,[lists:append(OrigPath,[H])]))
	end.

extend_path(H, Graph) ->
	{segment, SourceNode, DestNode, Dist} = lists:last(H),
	NextSteps = outgoing_paths(DestNode, Graph),
	build_extended_paths(NextSteps, H, [[]]).


extend_frontier([], Graph, ExtendedFrontier) -> ExtendedFrontier;
extend_frontier([H|FrontierTail], Graph, ExtendedFrontier) -> 
	ThisExtendedPath = extend_path(H, Graph),
	extend_frontier(FrontierTail, Graph, lists:append(ExtendedFrontier,ThisExtendedPath)).

init_path_to_array([],FirstPaths) -> FirstPaths;
init_path_to_array([H|InitPathsTail], FirstPaths) ->
	case FirstPaths of [] -> init_path_to_array(InitPathsTail, [[H]]);
					_Else -> init_path_to_array(InitPathsTail, lists:append(FirstPaths,[[H]]))
				end.

check_destination_reached([], EndNode) -> {ok, no_match};
check_destination_reached([H|FrontierTail],EndNode) ->
	{segment, SourceNode, DestNode, Dist} = lists:last(H),
	case DestNode of EndNode -> 
		{found,H};
		_Else ->
	check_destination_reached(FrontierTail,EndNode)
	end.

while({found, Path}, Frontier, EndNode, Graph) -> 
	io:fwrite("~p~n", [Frontier]), 
	Path;
while({ok, no_match}, Frontier, EndNode, Graph)->
	NextFrontier = extend_frontier(Frontier, Graph, []),
	while(check_destination_reached(NextFrontier,EndNode),NextFrontier,EndNode,Graph).



runsearch(StartNode,EndNode,Graph) ->

	FirstPathsList = outgoing_paths(StartNode, Graph),
	InitFrontier = init_path_to_array(FirstPathsList,[]),

	while(check_destination_reached(InitFrontier,EndNode),InitFrontier, EndNode, Graph).














