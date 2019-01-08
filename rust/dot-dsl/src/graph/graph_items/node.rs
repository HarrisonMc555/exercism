#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    attrs: Vec<(String, String)>,
}

impl Node {
    pub fn new(_: &str) -> Self {
        Node {
            attrs: Vec::new(),
        }
    }

    pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
        self.attrs = attrs.iter().map(
            |&(s1, s2)| (String::from(s1), String::from(s2))).collect();
        self
    }
}
