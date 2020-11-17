use std::borrow::Borrow;

/// A munger which XORs a key with some data
#[derive(Clone)]
pub struct Xorcism<'a> {
    key: &'a [u8],
    index: usize,
}

/// For composability, it is important that `munge` returns an iterator compatible with its input.
///
/// However, `impl Trait` syntax can specify only a single non-auto trait.
/// Therefore, we define this output trait with generic implementations on all compatible types,
/// and return that instead.
pub trait MungeOutput: Iterator<Item = u8> + ExactSizeIterator {}
impl<T> MungeOutput for T where T: Iterator<Item = u8> + ExactSizeIterator {}

impl<'a> Xorcism<'a> {
    /// Create a new Xorcism munger from a key
    ///
    /// Should accept anything which has a cheap conversion to a byte slice.
    pub fn new<Key>(key: &'a Key) -> Xorcism<'a>
    where
        Key: AsRef<[u8]> + ?Sized,
    {
        let key = key.as_ref();
        if key.is_empty() {
            panic!("Cannot use empty key");
        }
        Xorcism { key, index: 0 }
    }

    /// XOR each byte of the input buffer with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    pub fn munge_in_place(&mut self, data: &mut [u8]) {
        for byte in data.iter_mut() {
            *byte ^= self.key[self.index];
            self.increment_index();
        }
    }

    /// XOR each byte of the data with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    ///
    /// Should accept anything which has a cheap conversion to a byte iterator.
    /// Shouldn't matter whether the byte iterator's values are owned or borrowed.
    pub fn munge<'b, Data>(&mut self, data: Data) -> impl 'b + MungeOutput
    where
        Data: IntoIterator,
        Data::Item: Borrow<u8> + 'b,
        Data::IntoIter: ExactSizeIterator + 'b,
        'a: 'b,
    {
        let data_iter = data.into_iter();
        let mut index = self.index;
        self.add_to_index(data_iter.len());
        let key = self.key;
        data_iter.map(move |byte| {
            let output = byte.borrow() ^ key[index];
            index = increment_index(key, index);
            output
        })
    }

    fn increment_index(&mut self) {
        self.add_to_index(1);
    }

    fn add_to_index(&mut self, len: usize) {
        self.index = add_to_index(self.key, self.index, len);
    }

}

fn increment_index(key: &[u8], index: usize) -> usize {
    add_to_index(key, index, 1)
}

fn add_to_index(key: &[u8], index: usize, len: usize) -> usize {
    (index + len) % key.len()
}
