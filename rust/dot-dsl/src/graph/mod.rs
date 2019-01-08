pub mod graph_items;
use crate::graph::graph_items::node::Node;

pub struct Graph {
    pub nodes : Vec<Node>,
    pub edges : Vec<u32>,
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

    pub fn with_nodes(& mut self, nodes: &[Node]) -> & mut Self {
        self.nodes = nodes.to_vec();
        self
    }
}

