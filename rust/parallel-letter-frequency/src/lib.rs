use crossbeam::thread;
use std::collections::HashMap;
use std::sync::Arc;

pub fn frequency(input: &[&str], mut worker_count: usize) -> HashMap<char, usize> {
    let data = Arc::new(input);
    let mut num_per_worker = input.len() / worker_count;
    if num_per_worker == 0 {
        num_per_worker = input.len();
        worker_count = 1;
    }
    println!("input: {:?}", input);
    thread::scope(|s| {
        let handles = (0..worker_count).map(|index| {
            let start_index = num_per_worker * index;
            let end_index = start_index + num_per_worker;
            let data_clone = Arc::clone(&data); // Clone Arc, bump reference count
            let slice = &data_clone[start_index..end_index];
            println!("Spawning thread to deal with: {:?}", slice);
            s.spawn(move |_| frequency_helper(slice))
        });

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
