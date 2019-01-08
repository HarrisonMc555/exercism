pub mod graph_items;

use std::collections::HashMap;
use crate::graph::graph_items::node::Node;
use crate::graph::graph_items::edge::Edge;

pub struct Graph {
    pub nodes : Vec<Node>,
    pub edges : Vec<Edge>,
    pub attrs : HashMap<String, String>,
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            attrs: HashMap::new(),
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

    pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
        self.attrs = attrs.iter().map(
            |&(s1, s2)| (String::from(s1), String::from(s2))).collect();
        self
    }
}
