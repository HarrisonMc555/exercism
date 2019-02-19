const BOOK_COST: u32 = 800;

const DISCOUNTS: [(usize, u32); 4] = [(5, 25), (4, 20), (3, 10), (2, 5)];
const PERCENT: u32 = 100;

pub fn lowest_price(books: &[u32]) -> u32 {
    let mut books: Vec<_> = books.iter().cloned().collect();
    books.sort_unstable_by(|a, b| b.cmp(a));
    best_discount(&books, &DISCOUNTS)
}

fn best_discount(books: &[u32], discounts: &[(usize, u32)]) -> u32 {
    eprintln!("best_discount:");
    eprintln!("\tbooks: {:?}", books);
    eprintln!("\tdiscounts: {:?}", discounts);
    if discounts.is_empty() {
        return BOOK_COST * books.len() as u32;
    }
    let rem_discounts = &discounts[1..];
    let mut best_price = best_discount(books, rem_discounts);
    eprintln!("Best price from other discounts: {}", best_price);
    let (num_books, discount_percent) = discounts[0];
    let mut books: Vec<u32> = books.to_vec();
    while let Some((rem_books, price)) =
        discounted_price(num_books, discount_percent, &books)
    {
        eprintln!("Got a discount with ({}, {})", num_books, discount_percent);
        
        // let this_price = price - best_discount(&rem_books, rem_discounts);
        // let the_best_discount = best_discount(&rem_books, rem_discounts);
        // eprintln!("price: {}, best_discount: {}", price, the_best_discount);
        // let this_price = price - the_best_discount;
        // eprintln!("Price: {}", this_price);
        let this_price = best_discount(&rem_books, rem_discounts);
        
        if this_price < best_price {
            eprintln!("\tBetter than last price! ({})", best_price);
            best_price = this_price;
        }
        books = rem_books;
    }
    best_price
}

fn discounted_price(
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
        let discount = cost * discount_percent / PERCENT;
        eprintln!("cost: {}, discount: {}", cost, discount);
        // let discounted_cost = cost - discount;
        // eprintln!("discounted_cost: {}", discounted_cost);
        // return Some((all_remaining_books, discounted_cost));
        return Some((all_remaining_books, discount));
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
