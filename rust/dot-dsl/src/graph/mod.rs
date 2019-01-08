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
            |&(s1, s2)| (s1.to_owned(), s2.to_owned())).collect();
        self
    }

    pub fn get_node(&self, node_name: &str) -> Option<&Node> {
        for node in self.nodes.iter() {
            if node.get_name() == node_name {
                return Some(&node);
            }
        }
        None
    }

    pub fn get_edge(&self, source_name: &str, dest_name: &str) -> Option<&Edge> {
        for edge in self.edges.iter() {
            if edge.get_source_name() == source_name &&
                edge.get_dest_name() == dest_name {
                return Some(&edge);
            }
        }
        None
    }

    pub fn get_attr(&self, attr_name: &str) -> Option<&str> {
        self.attrs.get(attr_name).map(|r| r.as_str())
    }
}
