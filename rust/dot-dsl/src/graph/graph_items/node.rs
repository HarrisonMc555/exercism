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
            |&(s1, s2)| (s1.to_owned(), s2.to_owned())).collect();
        self
    }
}
