use std::collections::HashMap;
use crossbeam::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let num_per_worker = get_worker_size(input, worker_count);
    println!("input: {:?}", input);
    if input.is_empty() {
        return HashMap::new();
    }
    thread::scope(|s| {
        let handles = input
            .chunks(num_per_worker)
            .map(|slice| s.spawn(move |_| frequency_helper(slice)));
        let mut result = HashMap::new();
        for handle in handles {
            let partial_result = handle.join().unwrap();
            println!("partial_result: {:?}", partial_result);
            combine_frequencies(&mut result, partial_result);
        }
        println!("result: {:?}", result);
        result
    })
    .unwrap_or_else(|e| {
        eprintln!("Error: {:?}", e);
        HashMap::new()
    })
}

fn frequency_helper(input: &[&str]) -> HashMap<char, usize> {
    let mut frequencies = HashMap::new();
    for string in input {
        for letter in string
            .chars()
            .filter(|c| c.is_alphabetic())
            .map(|c| c.to_ascii_lowercase())
        {
            let counter = frequencies.entry(letter).or_insert(0);
            *counter += 1;
        }
    }
    frequencies
}

fn combine_frequencies(result: &mut HashMap<char, usize>, partial_result: HashMap<char, usize>) {
    for (letter, count) in partial_result {
        let result_counter = result.entry(letter).or_insert(0);
        *result_counter += count;
    }
}

fn get_worker_size<T>(data: &[T], worker_count: usize) -> usize {
    let length = data.len() as f64;
    let worker_count = worker_count as f64;
    let num_per_worker = math::round::ceil(length / worker_count, 0) as usize;
    if num_per_worker == 0 {
        return data.len();
    }
    num_per_worker
}
