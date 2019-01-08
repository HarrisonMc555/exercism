use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    attrs: HashMap<String, String>,
}

impl Edge {
    pub fn new(_: &str, _: &str) -> Self {
        Edge {
            attrs: HashMap::new(),
        }
    }

    pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
        self.attrs = attrs.iter().map(
            |&(s1, s2)| (s1.to_owned(), s2.to_owned())).collect();
        self
    }
}
