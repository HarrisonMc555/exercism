fn main() {
    let set = CustomSet::new(&[1, 2, 3]);
    for e in set.into_iter() {
        println!("{:?}", e);
    }
}
