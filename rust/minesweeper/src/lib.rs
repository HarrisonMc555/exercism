const BASE: u32 = 10;
const MINE: char = '*';
const BLANK: char = ' ';

type Grid<'a> = &'a [Vec<char>];
type Location = (usize, usize);

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let minefield = to_grid_of_chars(minefield);
    let row_indices = 0..minefield.len();
    row_indices.map(|i| annotate_row(&minefield, i)).collect()
}

fn annotate_row(minefield: Grid, row_i: usize) -> String {
    let col_indices = 0..minefield[row_i].len();
    col_indices
        .map(|j| annotate_cell(minefield, (row_i, j)))
        .collect()
}

fn annotate_cell(minefield: Grid, location: Location) -> char {
    let (row, col): (usize, usize) = location;
    let c = minefield[row][col];
    if is_mine(c) {
        c
    } else if is_blank(c) {
        let num_touching_mines = count_touching_mines(minefield, location);
        if num_touching_mines == 0 {
            BLANK
        } else {
            get_digit_char(num_touching_mines)
        }
    } else {
        panic!("Invalid minefield char")
    }
}

fn count_touching_mines(minefield: Grid, location: Location) -> u8 {
    let (row, col) = location;
    let min_row = 0;
    let first_row = if row == min_row { min_row } else { row - 1 };
    let max_row = minefield.len() - 1;
    let last_row = if row == max_row { max_row } else { row + 1 };
    let min_col = 0;
    let first_col = if col == min_col { min_col } else { col - 1 };
    let max_col = minefield[row].len() - 1;
    let last_col = if col == max_col { max_col } else { col + 1 };
    let touching_locations = (first_row..=last_row)
        .map(|i| (first_col..=last_col).map(move |j| (i, j)))
        .flatten();
    let mine_locations =
        touching_locations.filter(|&(i, j)| is_mine(minefield[i][j]));
    mine_locations.count() as u8
}

fn to_grid_of_chars(grid: &[&str]) -> Vec<Vec<char>> {
    grid.iter()
        .map(|&row_str| row_str.chars().collect())
        .collect()
}

fn is_mine(c: char) -> bool {
    c == MINE
}

fn is_blank(c: char) -> bool {
    c == BLANK
}

fn get_digit_char(digit: u8) -> char {
    std::char::from_digit(digit as u32, BASE).unwrap()
}
