#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Tile {
    Corner,
    Horizontal,
    Vertical,
    Empty,
}

type Grid = Vec<Vec<Tile>>;
type GridRef<'a> = &'a [Vec<Tile>];
type Location = (usize, usize);

pub fn count(lines: &[&str]) -> u32 {
    let tiles = match to_tiles(lines) {
        Some(tiles) => tiles,
        None => return 0,
    };
    count_rectangles(&tiles)
}

fn count_rectangles(tiles: GridRef) -> u32 {
    if tiles.is_empty() {
        return 0;
    }
    let row_indices = 0..tiles.len();
    let num_rectangles_from_rows =
        row_indices.map(|i| count_rectangles_from_row(tiles, i));
    num_rectangles_from_rows.sum()
}

fn count_rectangles_from_row(tiles: GridRef, row_index: usize) -> u32 {
    let col_indices_and_tiles = tiles[row_index].iter().enumerate();
    let top_left_corners =
        col_indices_and_tiles.filter(|(_, tile)| tile.is_corner());
    let num_rectangles_from_corners = top_left_corners
        .map(|(j, _)| count_rectangles_from_top_left(tiles, (row_index, j)));
    num_rectangles_from_corners.sum()
}

fn count_rectangles_from_top_left(tiles: GridRef, location: Location) -> u32 {
    let (row_index, col_index) = location;
    let col_indices = col_index + 1..tiles[0].len();
    let col_indices_and_tiles = col_indices.map(|j| (j, tiles[row_index][j]));
    let connected_horizontally = col_indices_and_tiles
        .take_while(|(_, tile)| tile.connects_horizontally());
    let connected_corners =
        connected_horizontally.filter(|(_, tile)| tile.is_corner());
    let num_rectangles_from_corners = connected_corners
        .map(|(j, _)| count_rectangles_from_top_right(tiles, j, location));
    num_rectangles_from_corners.sum()
}

fn count_rectangles_from_top_right(
    tiles: GridRef,
    col_index: usize,
    original_corner: Location,
) -> u32 {
    let (row_index, _) = original_corner;
    let row_indices = row_index + 1..tiles.len();
    let row_indices_and_tiles = row_indices.map(|i| (i, tiles[i][col_index]));
    let connected_vertically = row_indices_and_tiles
        .take_while(|(_, tile)| tile.connects_vertically());
    let corners_of_rectangles = connected_vertically.filter(|&(i, tile)| {
        let location = (i, col_index);
        tile.is_corner()
            && does_connect_from_bottom_right(tiles, location, original_corner)
    });
    let num_rectangles = corners_of_rectangles.count();
    num_rectangles as u32
}

fn does_connect_from_bottom_right(
    tiles: GridRef,
    location: Location,
    original_corner: Location,
) -> bool {
    let (row_index, col_index) = location;
    let (_, original_col) = original_corner;
    let horizontal_until_corner = (original_col + 1..col_index)
        .map(|j| tiles[row_index][j])
        .all(|tile| tile.connects_horizontally());
    if !horizontal_until_corner {
        return false;
    }
    let bottom_left_is_corner = tiles[row_index][original_col].is_corner();
    if !bottom_left_is_corner {
        return false;
    }
    if !does_connect_from_bottom_left(tiles, row_index, original_corner) {
        return false;
    }
    true
}

fn does_connect_from_bottom_left(
    tiles: GridRef,
    row_index: usize,
    original_corner: Location,
) -> bool {
    let (original_row, col_index) = original_corner;
    let col_indices = original_row..row_index;
    // We assume we originally started from a corner just like we assume each
    // call to does_connect_from_bottom_left starts from a corner
    col_indices
        .map(|i| tiles[i][col_index])
        .all(|tile| tile.connects_vertically())
}

fn to_tiles(lines: &[&str]) -> Option<Grid> {
    lines
        .iter()
        .map(|line| line.chars().map(Tile::try_from_char).collect())
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

    pub fn is_corner(self) -> bool {
        match self {
            Tile::Corner => true,
            _ => false,
        }
    }

    pub fn connects_horizontally(self) -> bool {
        match self {
            Tile::Corner => true,
            Tile::Horizontal => true,
            _ => false,
        }
    }

    pub fn connects_vertically(self) -> bool {
        match self {
            Tile::Corner => true,
            Tile::Vertical => true,
            _ => false,
        }
    }
}
