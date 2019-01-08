pub mod graph_items;

use crate::graph::graph_items::node::Node;
use crate::graph::graph_items::edge::Edge;

pub struct Graph {
    pub nodes : Vec<Node>,
    pub edges : Vec<Edge>,
    pub attrs : Vec<u32>,
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            attrs: Vec::new(),
        }
    }

    pub fn with_nodes(mut self, nodes: &[Node]) -> Self {
        self.nodes = nodes.to_vec();
        self
    }

    pub fn with_edges(mut self, edges: &[Edge]) -> Self {
        self.edges = edges.to_vec();
        self
    }
}
