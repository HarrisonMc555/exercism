use multiset::HashMultiSet;
use std::hash::Hash;

type Domino<T> = (T, T);
type Solution<T> = Vec<Domino<T>>;

pub fn chain(input: &[Domino<u8>]) -> Option<Solution<u8>> {
    if input.is_empty() {
        return Some(Vec::new());
    }
    let starting_domino = input[0];
    let chain = Chain::new(vec![starting_domino]);
    let remaining: HashMultiSet<_> = input[1..].iter().cloned().collect();
    find_solution(chain, remaining)
}

fn find_solution<T>(
    mut cur_chain: Chain<T>,
    mut remaining: HashMultiSet<Domino<T>>,
) -> Option<Solution<T>>
where
    T: Ord + Clone + Hash,
{
    if remaining.is_empty() {
        return cur_chain.get_solution();
    }
    let remaining_vec: Vec<_> = remaining.iter().cloned().collect();
    let tail = cur_chain.tail().clone();
    for domino in remaining_vec {
        if let Some(oriented) = oriented_match(&domino, &tail) {
            remaining.remove(&domino);
            cur_chain.push(oriented);
            if let Some(solution) =
                find_solution(cur_chain.clone(), remaining.clone())
            {
                return Some(solution);
            }
            cur_chain.pop();
            remaining.insert(domino);
        }
    }
    None
}

fn oriented_match<T: Eq + Clone>(
    domino: &Domino<T>,
    pip: &T,
) -> Option<Domino<T>> {
    if &domino.0 == pip {
        Some(domino.clone())
    } else if &domino.1 == pip {
        Some((domino.1.clone(), domino.0.clone()))
    } else {
        None
    }
}

#[derive(Clone)]
struct Chain<T> {
    dominoes: Vec<Domino<T>>,
}

impl<T: Ord> Chain<T> {
    pub fn new(dominoes: Vec<Domino<T>>) -> Self {
        assert!(!dominoes.is_empty());
        Chain { dominoes }
    }

    pub fn head(&self) -> &T {
        &self.dominoes[0].0
    }

    pub fn tail(&self) -> &T {
        &self.dominoes[self.dominoes.len() - 1].1
    }

    pub fn is_valid_loop(&self) -> bool {
        self.head() == self.tail()
    }

    pub fn get_solution(self) -> Option<Solution<T>> {
        if self.is_valid_loop() {
            Some(self.dominoes)
        } else {
            None
        }
    }

    pub fn push(&mut self, domino: Domino<T>) {
        self.dominoes.push(domino);
    }

    pub fn pop(&mut self) -> Option<Domino<T>> {
        self.dominoes.pop()
    }
}
