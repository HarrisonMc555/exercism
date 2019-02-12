const BOOK_COST: u32 = 800;

const DISCOUNTS: [(usize, u32); 4] = [
    (5, 25),
    (4, 20),
    (3, 10),
    (2, 5),
];

pub fn lowest_price(books: &[u32]) -> u32 {
    0
}

fn best_discount(books: &[u32], discounts: &[(usize, u32)]) -> u32 {
    0
}

fn discount(
    num: usize,
    discount_percent: u32,
    books: &[u32],
) -> (Vec<u32>, u32) {
    if books.len() < num {
        return (books.iter().cloned().collect(), 0);
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
        return (all_remaining_books, discounted_cost);
    }
    (books.iter().cloned().collect(), 0)
}

fn at_least_n_matching<I, F, T>(iterable: I, pred: F, count: usize) -> bool
where
    I: IntoIterator<Item = T>,
    F: Fn(&T) -> bool,
{
    iterable.into_iter().filter(pred).take(count).count() >= count
}
