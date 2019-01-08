use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    source_name: String,
    dest_name: String,
    attrs: HashMap<String, String>,
}

impl Edge {
    pub fn new(source_name: &str, dest_name: &str) -> Self {
        Edge {
            source_name: source_name.to_owned(),
            dest_name: dest_name.to_owned(),
            attrs: HashMap::new(),
        }
    }

    pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
        self.attrs = attrs.iter().map(
            |&(s1, s2)| (s1.to_owned(), s2.to_owned())).collect();
        self
    }

    pub fn get_source_name(&self) -> &str {
        &self.source_name
    }

    pub fn get_dest_name(&self) -> &str {
        &self.dest_name
    }

    pub fn get_attr(&self, attr_name: &str) -> Option<&str> {
        self.attrs.get(attr_name).map(|r| r.as_str())
    }
}
