use itertools::Itertools;

const BOOK_COST: u32 = 800;

const DISCOUNTS: [(usize, u32); 4] = [(5, 25), (4, 20), (3, 10), (2, 5)];
const PERCENT: u32 = 100;

pub fn lowest_price(books: &[u32]) -> u32 {
    let mut book_counts = get_book_counts(books);
    sort_filter_book_counts(&mut book_counts);
    // best_discount(&books, &DISCOUNTS)
    let price = best_discount(&book_counts, &DISCOUNTS);
    eprintln!("Total lowest price: {}", price);
    price
}

fn best_discount(book_counts: &[u32], discounts: &[(usize, u32)]) -> u32 {
    let indent: String = (0..4 - discounts.len()).map(|_| "  ").collect();
    eprintln!("{}best_discount:", indent);
    eprintln!("{}books: {:?}", indent, book_counts);
    eprintln!("{}discounts: {:?}", indent, discounts);
    if discounts.is_empty() {
        let num_books: u32 = book_counts.iter().sum();
        eprintln!(
            "{}no discounts, full price for all {} books",
            indent,
            num_books
        );
        return BOOK_COST * num_books as u32;
    }
    let rem_discounts = &discounts[1..];
    let mut best_price = best_discount(book_counts, rem_discounts);
    eprintln!("{}Best price from other discounts: {}", indent, best_price);
    let (num_books, discount_percent) = discounts[0];
    let mut books: Vec<u32> = book_counts.to_vec();
    while let Some((rem_books, discounted_price)) =
        get_discounted_price(num_books, discount_percent, &books)
    {
        eprintln!(
            "{}Got a discount with ({}, {})",
            indent, num_books, discount_percent
        );
        eprintln!("{}Discounted price is {}", indent, discounted_price);
        // let this_price = price - best_discount(&rem_books, rem_discounts);
        // let the_best_discount = best_discount(&rem_books, rem_discounts);
        // eprintln!("price: {}, best_discount: {}", price, the_best_discount);
        // let this_price = price - the_best_discount;
        // eprintln!("Price: {}", this_price);

        let price_of_rest = best_discount(&rem_books, rem_discounts);
        let this_total_price = discounted_price + price_of_rest;
        if this_total_price < best_price {
            eprintln!(
                "{}{} is better than last price! ({})",
                indent, this_total_price, best_price
            );
            best_price = this_total_price;
        }
        books = rem_books;
    }
    eprintln!(
        "{}Best price for {} books with {} discounts: {}",
        indent,
        books.len(),
        discounts.len(),
        best_price
    );
    best_price
}

fn get_discounted_price(
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
        let mut all_remaining_books: Vec<_> =
            remaining_discounted_books.chain(other_books).collect();
        // all_remaining_books.sort_unstable_by(|a, b| b.cmp(a));
        sort_filter_book_counts(&mut all_remaining_books);
        let cost = BOOK_COST * num as u32;
        let discount = cost * discount_percent / PERCENT;
        eprintln!("cost: {}, discount: {}", cost, discount);
        let discounted_cost = cost - discount;
        eprintln!("discounted_cost: {}", discounted_cost);
        return Some((all_remaining_books, discounted_cost));
        // return Some((all_remaining_books, discount));
    }
    None
}

fn sort_filter_book_counts(book_counts: &mut Vec<u32>) {
    book_counts.retain(|&count| count != 0);
    book_counts.sort_unstable_by(|a, b| b.cmp(a));
}

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
