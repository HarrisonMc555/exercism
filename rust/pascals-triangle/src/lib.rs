pub struct PascalsTriangle {
    row_count: u32,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        PascalsTriangle { row_count }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        let mut rows = vec![];
        if self.row_count < 1 {
            return rows;
        }
        rows.push(vec![1]);
        let mut last_row = vec![1];
        let mut cur_row: Vec<u32>;
        for _ in 1..self.row_count {
            cur_row = PascalsTriangle::next_row(&last_row);
            rows.push(cur_row.clone());
            last_row = cur_row;
        }
        rows
    }

    fn next_row(row: &[u32]) -> Vec<u32> {
        if row.is_empty() {
            return vec![1];
        }
        let mut next_row = Vec::with_capacity(row.len() + 1);
        next_row.push(1);
        for i in 0..row.len() - 1 {
            next_row.push(row[i] + row[i + 1]);
        }
        next_row.push(1);
        next_row
    }
}
