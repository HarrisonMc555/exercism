pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut saddle_points = Vec::new();
    for (row_index, row) in input.iter().enumerate() {
        for (col_index, _) in row.iter().enumerate() {
            let indices = (row_index, col_index);
            if is_saddle_point(indices, input) {
                saddle_points.push(indices);
            }
        }
    }
    saddle_points
}

fn is_saddle_point(indices: (usize, usize), arr: &[Vec<u64>]) -> bool {
    let (row_index, col_index) = indices;
    let val = arr[row_index][col_index];
    greater_than_row(val, row_index, arr) && less_than_col(val, col_index, arr)
}

fn greater_than_row(val: u64, row_index: usize, arr: &[Vec<u64>]) -> bool {
    arr[row_index].iter().all(|e| *e <= val)
}

fn less_than_col(val: u64, col_index: usize, arr: &[Vec<u64>]) -> bool {
    arr.iter().map(|row| row[col_index]).all(|e| e >= val)
}
