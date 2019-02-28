#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Tile {
    Corner,
    Horizontal,
    Vertical,
    Empty,
}

type Grid = Vec<Vec<Tile>>;
type Location = (usize, usize);

pub fn count(lines: &[&str]) -> u32 {
    let tiles = match to_tiles(lines) {
        Some(tiles) => tiles,
        None => return 0,
    };
    count_rectangles(tiles)
}

fn count_rectangles(tiles: Grid) -> u32 {
    if tiles.is_empty() {
        return 0;
    }
    tiles
        .iter()
        .enumerate()
        .flat_map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|(_, tile)| tile.is_corner())
                .map(|(j, _)| count_rectangles_from_top_left(&tiles, i, j))
                .collect::<Vec<_>>()
        })
        .sum()
}

fn count_rectangles_from_top_left(
    tiles: &Grid,
    row_index: usize,
    col_index: usize,
) -> u32 {
    let original_corner = (row_index, col_index);
    (col_index + 1..tiles[0].len())
        .map(|j| (j, tiles[row_index][j]))
        .take_while(|(_, tile)| tile.connects_horizontally())
        .filter(|(_, tile)| tile.is_corner())
        .map(|(j, _)| {
            count_rectangles_from_top_right(
                tiles,
                row_index,
                j,
                original_corner,
            )
        })
        .sum()
}

fn count_rectangles_from_top_right(
    tiles: &Grid,
    row_index: usize,
    col_index: usize,
    original_corner: Location,
) -> u32 {
    (row_index + 1..tiles.len())
        .map(|i| (i, tiles[i][col_index]))
        .take_while(|(_, tile)| tile.connects_vertically())
        .filter(|(_, tile)| tile.is_corner())
        .map(|(i, _)| {
            count_rectangles_from_bottom_right(
                tiles,
                i,
                col_index,
                original_corner,
            )
        })
        .sum()
}

fn count_rectangles_from_bottom_right(
    tiles: &Grid,
    row_index: usize,
    col_index: usize,
    original_corner: Location,
) -> u32 {
    let (_, original_col) = original_corner;
    let horizontal_until_corner = (original_col + 1..col_index)
        .map(|j| tiles[row_index][j])
        .all(|tile| tile.connects_horizontally());
    let corner_is_corner = tiles[row_index][original_col].is_corner();
    let does_connect = horizontal_until_corner && corner_is_corner;
    if does_connect {
        count_rectangles_from_bottom_left(
            tiles,
            row_index,
            original_col,
            original_corner,
        )
    } else {
        0
    }
}

fn count_rectangles_from_bottom_left(
    tiles: &Grid,
    row_index: usize,
    col_index: usize,
    original_corner: Location,
) -> u32 {
    let (original_row, _) = original_corner;
    let does_connect = (original_row..row_index)
        .map(|i| tiles[i][col_index])
        .all(|tile| tile.connects_vertically());
    if does_connect {
        1
    } else {
        0
    }
}

fn to_tiles(lines: &[&str]) -> Option<Grid> {
    lines
        .iter()
        .map(|line| line.chars().map(|c| Tile::try_from_char(c)).collect())
        .collect()
}

impl Tile {
    pub fn try_from_char(c: char) -> Option<Self> {
        Some(match c {
            '+' => Tile::Corner,
            '-' => Tile::Horizontal,
            '|' => Tile::Vertical,
            ' ' => Tile::Empty,
            _ => return None,
        })
    }

    pub fn is_corner(&self) -> bool {
        match self {
            Tile::Corner => true,
            _ => false,
        }
    }

    pub fn connects_horizontally(&self) -> bool {
        match self {
            Tile::Corner => true,
            Tile::Horizontal => true,
            _ => false,
        }
    }

    pub fn connects_vertically(&self) -> bool {
        match self {
            Tile::Corner => true,
            Tile::Vertical => true,
            _ => false,
        }
    }
}
