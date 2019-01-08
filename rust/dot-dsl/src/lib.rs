pub mod graph {
    pub struct Graph {
        pub nodes : Vec<u32>,
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
    }
}
