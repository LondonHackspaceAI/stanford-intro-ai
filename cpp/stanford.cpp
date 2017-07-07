//stanford.cpp
#include <iostream>
#include <string>
#include <vector>
#include <map>
//#include <typeinfo>


class Node {

	std::string name;

public:
	Node() {};
	Node(std::string init_name) : name(init_name) {};
	~Node() {}; 

	std::string get_name () const {return name; };
};

class Segment {
	Node start;
	Node end;
	int dist;
public:
	Segment(std::string init_start, std::string init_end, int init_dist) {
		start = Node(init_start);
		end = Node(init_end);
		dist = init_dist;
	};
	~Segment() {};
	std::string get_start_name () const {return start.get_name(); };
	std::string get_end_name () const { return end.get_name(); };
	Node get_start_node () const { return start; };
	Node get_end_node () const { return end; };
	int get_dist () const { return dist; };
};

class BestPath {
	std::string to_node_name;
	std::vector<Segment> path;
	int total_dist;
public:
	BestPath(std::string node_name, std::vector<Segment> init_path, int init_dist) 
		: to_node_name(node_name), path(init_path), total_dist(init_dist) {};
	~BestPath() {};

	int get_dist() const {return total_dist; };
	void update_dist(int dist) {total_dist = dist; };
	void update_path(std::vector<Segment> update_path) {path = update_path; };
	std::string get_name() const {return to_node_name; };


};

void display_graph(std::vector<Segment> graph) {
		for ( int i = 0; i < graph.size(); i++ ) {
			std::cout << graph[i].get_start_name() << "   " 
				<< graph[i].get_end_name() << "   " 
				<< graph[i].get_dist() << std::endl;
		}
}

void display_frontier(std::vector< std::vector<Segment> > frontier) {
	for (int i = 0; i < frontier.size(); i++) {
		for(int j = 0; j < frontier[i].size(); j++ ) {
			std::cout << frontier[i][j].get_start_name() << "   "
				<< frontier[i][j].get_end_name() << "   "
				<< frontier[i][j].get_dist()
				<< ";   ";
		}
		std::cout << std::endl;
	}
}


std::vector<Segment> outgoing_paths(Node& node, std::vector<Segment>& graph) {

	std::string endnode_from = node.get_name();

	std::vector<Segment> retsegs;
	for ( int i = 0; i < graph.size(); i++ ) {
		std::string nextstartnode = graph[i].get_start_name();
		if(nextstartnode == endnode_from) {
			retsegs.push_back(graph[i]);
		}
	}

	return retsegs;
}

std::vector< std::vector<Segment> > extend_frontier(std::vector< std::vector<Segment> >& frontier, 
	std::vector<Segment>& graph,
	std::map<std::string, BestPath>* visited) {

	std::vector< std::vector<Segment> > retfrontier;
	while(!frontier.empty()) {
		std::cout << "while step" << std::endl;
		std::vector<Segment> nextpath = frontier.back();
		frontier.pop_back();
		Node termnode = nextpath[nextpath.size()-1].get_end_node();
		std::vector<Segment> nextsteps = outgoing_paths(termnode, graph);
		for(int i = 0; i < nextsteps.size(); i++) {
			std::cout << "nextstep step" << std::endl;

			std::map<std::string, BestPath>::iterator iter;
			iter = visited->find(nextsteps[i].get_end_name());
			//if we've never been here this will be the shortest route so far
			if(iter == visited->end()) {
				nextpath.push_back(nextsteps[i]);
				int path_total_dist = 0;
				for( int i = 0; i < nextpath.size(); i++ ) {
					path_total_dist += nextpath[i].get_dist();
				}
				visited->insert(std::pair<std::string, BestPath>(
					nextsteps[i].get_end_name(), BestPath(nextsteps[i].get_end_name(), 
						nextpath, path_total_dist)));
				retfrontier.push_back(nextpath);
				nextpath.pop_back();
			} else { //if we've already been here, then if this route is faster keep it
				nextpath.push_back(nextsteps[i]);
				int path_total_dist = 0;
				for( int i = 0; i < nextpath.size(); i++ ) {
					path_total_dist += nextpath[i].get_dist();
				}
				if( path_total_dist < iter->second.get_dist() ) {
					iter->second.update_path(nextpath);
					iter->second.update_dist(path_total_dist);
					retfrontier.push_back(nextpath);
					nextpath.pop_back();
				} 
				
			}
		}
	}


	return retfrontier;
}

bool check_target_reached(Node& target, std::vector< std::vector<Segment> > frontier) {

	for(int i = 0; i < frontier.size(); i++ ) {
		if (frontier[i].back().get_end_name() == target.get_name()) {
			return true;
		}

	}

	return false;
}


int main() {

	//build graph
	std::vector<Segment> mygraph = {
		Segment("Arad", "Zerind", 75),
		Segment("Arad", "Sibiu", 140),
		Segment("Arad", "Timisoara", 118),
		Segment("Zerind", "Dradea", 71),
		Segment("Dradea", "Sibiu", 151),
		Segment("Sibiu", "Fagaras", 99),
		Segment("Sibiu", "RimnicuVilcea", 80),
		Segment("Fagaras", "Bucharest", 211),
		Segment("RimnicuVilcea", "Pitesti", 97),
		Segment("RimnicuVilcea", "Craiova", 146),
		Segment("Pitesti", "Craiova", 138),
		Segment("Pitesti", "Bucharest", 101),
		Segment("Timisoara", "Lugoj", 111),
		Segment("Lugoj", "Mehadia", 70),
		Segment("Mehadia", "Drobeta", 75),
		Segment("Drobeta", "Craiova", 120),
		Segment("Bucharest", "Giurgiu", 90),
		Segment("Bucharest", "Urziceni", 85),
		Segment("Urziceni", "Hirsova", 98),
		Segment("Hirsova", "Eforie", 86),
		Segment("Urziceni", "Vaslui", 142),
		Segment("Vaslui", "Iasi", 92),
		Segment("Iasi", "Neamt", 87)
	};

	std::vector<Segment> reverses;
	for(int i = 0 ; i < mygraph.size() ; i++ ) {
		reverses.push_back(Segment(mygraph[i].get_end_name(), 
			mygraph[i].get_start_name(), 
			mygraph[i].get_dist()));

	}
	//make both ways
	for(int i = 0; i < reverses.size(); i++ ) {
		mygraph.push_back(reverses[i]);
	}

	//display_graph(mygraph);

	Node nodetest = mygraph[1].get_end_node();
	std::vector<Segment> nextsteps = outgoing_paths(nodetest, mygraph);
	display_graph(nextsteps);

	std::vector< std::vector<Segment> > frontier;
	Node startnode = Node("Arad");
	Node targetnode = Node("Neamt");

	std::vector<Segment> initfrontest = outgoing_paths(startnode, mygraph);
	for( int i =0; i < initfrontest.size(); i++) {
		std::vector<Segment> oneinitpath = {initfrontest[i]};
		frontier.push_back(oneinitpath);
	}

	//init visited
	std::map<std::string, BestPath> visitedtest;
	for(int i = 0; i < frontier.size(); i++) {
		BestPath fvpt = BestPath(frontier[i].back().get_end_name(), 
			frontier[i], frontier[i][0].get_dist());
		visitedtest.insert(std::pair<std::string, BestPath>(frontier[i].back().get_end_name(),
			fvpt));
	}



	Node testend = Node("Dradea");
	while(!check_target_reached(targetnode, frontier)) {
		frontier = extend_frontier(frontier, mygraph, &visitedtest);
		display_frontier(frontier);
		
		std::cout << "not reached" << std::endl;
	}
	display_frontier(frontier);
	std::cout << "end reached" << std::endl;
	//std::cout << typeid(visitedtest.find("Arad")->second).name() << std::endl;








	return 0;
}