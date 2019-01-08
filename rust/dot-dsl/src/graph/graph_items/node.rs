#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    name: String,
    attrs: Vec<(String, String)>,
}

impl Node {
    pub fn new(name: &str) -> Self {
        Node {
            name: name.to_owned(),
            attrs: Vec::new(),
        }
    }

    pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
        self.attrs = attrs.iter().map(
            |&(s1, s2)| (s1.to_owned(), s2.to_owned())).collect();
        self
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}
