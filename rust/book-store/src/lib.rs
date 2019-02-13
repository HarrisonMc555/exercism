const BOOK_COST: u32 = 800;

const DISCOUNTS: [(usize, u32); 4] = [
    (5, 25),
    (4, 20),
    (3, 10),
    (2, 5),
];

pub fn lowest_price(books: &[u32]) -> u32 {
    best_discount(books, &DISCOUNTS)
}

fn best_discount(books: &[u32], discounts: &[(usize, u32)]) -> u32 {
    if discounts.is_empty() {
        return BOOK_COST * books.len() as u32;
    }
    let rem_discounts = &discounts[1..];
    let mut best_price = best_discount(books, rem_discounts);
    let (num_books, discount_percent) = discounts[0];
    let mut books: Vec<u32> = books.to_vec();
    while let Some((rem_books, price)) = discount(num_books, discount_percent, &books)
    {
        let this_price = price + best_discount(&rem_books, rem_discounts);
        if this_price < best_price {
            best_price = this_price;
        }
        books = rem_books;
    }
    best_price
}

fn discount(
    num: usize,
    discount_percent: u32,
    books: &[u32],
) -> Option<(Vec<u32>, u32)> {
    if books.len() < num {
        return None;
    }
    let books_to_discount = &books[..num];
    if books_to_discount.iter().all(|&c| c >= 1) {
        let remaining_discounted_books =
            books_to_discount.iter().map(|c| c - 1);
        let other_books = books[num..].iter().cloned();
        let all_remaining_books =
            remaining_discounted_books.chain(other_books).collect();
        let cost = BOOK_COST * num as u32;
        let discount = cost * discount_percent;
        let discounted_cost = cost - discount;
        return Some((all_remaining_books, discounted_cost));
    }
    None
}

// fn at_least_n_matching<I, F, T>(iterable: I, pred: F, count: usize) -> bool
// where
//     I: IntoIterator<Item = T>,
//     F: Fn(&T) -> bool,
// {
//     iterable.into_iter().filter(pred).take(count).count() >= count
// }
