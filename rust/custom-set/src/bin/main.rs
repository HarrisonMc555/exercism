extern crate custom_set;

use custom_set::CustomSet;

fn main() {
    let set = CustomSet::new(&[1, 2, 3]);
    let mut i = 0;
    for e in &set {
        println!("{:?}", e);
        i += 1;
        if i > 7 {
            break;
        }
    }
}
