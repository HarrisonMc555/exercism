use itertools::Itertools;

const BOOK_COST: u32 = 800;

const DISCOUNTS: [(usize, u32); 4] = [(5, 25), (4, 20), (3, 10), (2, 5)];
const PERCENT: u32 = 100;

pub fn lowest_price(books: &[u32]) -> u32 {
    let mut book_counts = get_book_counts(books);
    sort_filter_book_counts(&mut book_counts);
    best_price_with_discounts(&book_counts, &DISCOUNTS)
}

fn best_price_with_discounts(
    book_counts: &[u32],
    discounts: &[(usize, u32)],
) -> u32 {
    if discounts.is_empty() {
        return get_full_price(book_counts);
    }
    let (num_books, discount_percent) = discounts[0];
    let rem_discounts = &discounts[1..];

    // Initialize the current best price to the price you would get without the
    // current discount
    let mut best_price = best_price_with_discounts(book_counts, rem_discounts);
    let mut book_counts: Vec<u32> = book_counts.to_vec();

    // Keep track of the price of the previous books you've bought with this
    // discount
    let mut price_for_previous_books = 0;

    // Keep applying this discount again and again until we don't have enough
    // books to apply this discount any more
    while let Some((rem_books, cur_discounted_price)) =
        get_discounted_price(num_books, discount_percent, &book_counts)
    {
        let price_of_remaining =
            best_price_with_discounts(&rem_books, rem_discounts);
        let this_total_price = price_for_previous_books
            + cur_discounted_price
            + price_of_remaining;

        if this_total_price < best_price {
            best_price = this_total_price;
        }

        book_counts = rem_books;
        price_for_previous_books += cur_discounted_price;
    }

    best_price
}

fn get_discounted_price(
    num: usize,
    discount_percent: u32,
    book_counts: &[u32],
) -> Option<(Vec<u32>, u32)> {
    // You can only use the discount if you have at least `num` books
    if book_counts.len() < num {
        return None;
    }
    
    let books_to_discount = &book_counts[..num];
    // You need to have at least one of each book
    // The current implementation removes zeros, so this should never happen
    if !books_to_discount.iter().all(|&c| c >= 1) {
        return None;
    }

    // We've applied a discount to some of these books, but there may be others
    // remaining that didn't get discounted
    let remaining_discounted_books =
        books_to_discount.iter().map(|c| c - 1);
    let other_books = book_counts[num..].iter().cloned();

    // We return the books that did not get discounted. The books that we
    // discounted were removed so they're not discounted again.
    let mut all_remaining_books: Vec<_> =
        remaining_discounted_books.chain(other_books).collect();
    sort_filter_book_counts(&mut all_remaining_books);

    // This is what the cost would have been if they were full price
    let cost = BOOK_COST * num as u32;
    let discount = cost * discount_percent / PERCENT;
    let discounted_cost = cost - discount;
    Some((all_remaining_books, discounted_cost))
}

// Sort books counts from high to low, removing zeros
fn sort_filter_book_counts(book_counts: &mut Vec<u32>) {
    book_counts.retain(|&count| count != 0);
    book_counts.sort_unstable_by(|a, b| b.cmp(a));
}

// Return the number of each book in `books` (without preserving the book "id")
fn get_book_counts(books: &[u32]) -> Vec<u32> {
    let mut books = books.to_vec();
    books.sort_unstable();
    books
        .iter()
        .group_by(|&x| x)
        .into_iter()
        .map(|(_, group)| group.count() as u32)
        .collect()
}

fn get_full_price(book_counts: &[u32]) -> u32 {
    let num_books: u32 = book_counts.iter().sum();
    BOOK_COST * num_books
}
