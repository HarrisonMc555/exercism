pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut saddle_points = Vec::new();
    for row_index in 0..input.len() {
        for col_index in 0..input[row_index].len() {
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
    greater_than_row(val, row_index, arr)
        && less_than_col(val, col_index, arr)
}

fn greater_than_row(val: u64, row_index: usize, arr: &[Vec<u64>]) -> bool {
    for &other in arr[row_index].iter() {
        if other > val {
            return false;
        }
    }
    true
}

fn less_than_col(val: u64, col_index: usize, arr: &[Vec<u64>]) -> bool {
    for row in arr.iter() {
        let other = row[col_index];
        if other < val {
            return false;
        }
    }
    true
}
