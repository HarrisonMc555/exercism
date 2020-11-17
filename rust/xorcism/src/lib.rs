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

// struct Munger<'a> {
//     xorcism: Xorcism<'a>,
//     data: &'a [u8],
//     index: usize,
// }

// impl MungeOutput for Munger {
//     fn next(&mut self) -> Option<Self::Item> {

//     }
// }

impl<'a> Xorcism<'a> {
    /// Create a new Xorcism munger from a key
    ///
    /// Should accept anything which has a cheap conversion to a byte slice.
    pub fn new<Key>(key: &'a Key) -> Xorcism<'a>
    where
        Key: AsRef<[u8]> + ?Sized,
    {
        let key = key.as_ref();
        if key.len() <= 0 {
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
            self.index = (self.index + 1) % self.key.len();
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
        let key_iter = self
            .key
            .iter()
            .cycle()
            .skip(self.index)
            .take((&data_iter).len())
            // .copied()
            // .cloned()
            .collect::<Vec<_>>();
        self.index = (self.index + data_iter.len()) % self.key.len();
        // let key_iter = self
        //     .key
        //     .iter()
        //     .cycle()
        //     .skip(self.index)
        //     .take(data_iter.len());
        // data.into_iter()
        data_iter
            .zip(key_iter)
            .map(move |(data_byte, key_byte)| key_byte ^ data_byte.borrow())

        // data.into_iter().map(move |byte| {
        //     let output = byte.borrow() ^ self.key[self.index];
        //     self.index = (self.index + 1) % self.key.len();
        //     output
        // })
        // Munger {
        //     xorcism: self,
        //     iter: data.into_iter(),
        // }
    }
}

// struct Munger<'a, T>
// where
//     T: Iterator + ExactSizeIterator,
//     T::Item: Borrow<u8>,
// {
//     xorcism: &'a mut Xorcism<'a>,
//     iter: T,
// }

// impl<'a, T> Iterator for Munger<'a, T>
// where
//     T: Iterator + ExactSizeIterator,
//     T::Item: Borrow<u8>,
// {
//     type Item = u8;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter.next().map(|byte| {
//             let output = byte.borrow() ^ self.xorcism.key[self.xorcism.index];
//             self.xorcism.index = (self.xorcism.index + 1) % self.xorcism.key.len();
//             output
//         })
//     }
// }

// impl<'a, T> ExactSizeIterator for Munger<'a, T>
// where
//     T: Iterator + ExactSizeIterator,
//     T::Item: Borrow<u8>,
// {
// }
