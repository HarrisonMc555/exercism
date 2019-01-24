pub fn spiral_matrix(size: u32) -> Vec<Vec<u32>> {
    let max = size * size;
    let size = size as usize;
    let mut matrix = vec![vec![0; size]; size];
    let (mut di, mut dj): (isize, isize) = (0, 1);
    let (mut i, mut j) = (0, 0);
    for x in 1..=max {
        matrix[i][j] = x;
        let ((nexti, nextj), (nextdi, nextdj)) =
            next_ij_didj(&matrix, (i, j), (di, dj));
        i = nexti;
        j = nextj;
        di = nextdi;
        dj = nextdj;
    }
    matrix
}

fn next_didj((di, dj): (isize, isize)) -> (isize, isize) {
    match (di, dj) {
        (0, 1) => (1, 0),
        (1, 0) => (0, -1),
        (0, -1) => (-1, 0),
        (-1, 0) => (0, 1),
        _ => panic!("Invalid didj value"),
    }
}

fn next_ij_didj(
    matrix: &[Vec<u32>],
    (i, j): (usize, usize),
    (di, dj): (isize, isize),
) -> ((usize, usize), (isize, isize)) {
    let nexti = i as isize + di;
    let nextj = j as isize + dj;
    if invalid_indices((nexti, nextj), &matrix) {
        let (nextdi, nextdj) = next_didj((di, dj));
        (
            (add_signed(i, nextdi), add_signed(j, nextdj)),
            (nextdi, nextdj),
        )
    } else {
        ((nexti as usize, nextj as usize), (di, dj))
    }
}

fn invalid_indices((i, j): (isize, isize), matrix: &[Vec<u32>]) -> bool {
    let size = matrix.len() as isize;
    i < 0
        || i >= size
        || j < 0
        || j >= size
        || matrix[i as usize][j as usize] != 0
}

fn add_signed(x: usize, y: isize) -> usize {
    if y < 0 {
        x - -y as usize
    } else {
        x + y as usize
    }
}
