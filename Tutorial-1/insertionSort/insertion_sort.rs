use std::time::Instant;

fn create_array(size: usize) -> Vec<usize> {
    let mut arr = Vec::with_capacity(size);
    for i in 0..size {
        arr.push(i * i);
    }
    arr
}

fn insertion_sort(arr: &mut [usize]) {
    let n = arr.len();
    for i in 1..n {
        let key = arr[i];
        let mut j = i;
        while j > 0 && arr[j - 1] > key {
            arr[j] = arr[j - 1];
            j -= 1;
        }
        arr[j] = key;
    }
}

fn main() {
    const START_SIZE: usize = 10000;
    const END_SIZE: usize = 100000;
    const STEP_SIZE: usize = 10000;

    for list_size in (START_SIZE..=END_SIZE).step_by(STEP_SIZE) {
        let mut numbers = create_array(list_size);

        let start = Instant::now();

        insertion_sort(&mut numbers);

        let duration = start.elapsed();

        println!("Rust List Size: {}, Time taken: {:.6} seconds", list_size, duration.as_secs_f64());
    }
}
