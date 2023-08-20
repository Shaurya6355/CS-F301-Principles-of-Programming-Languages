use std::time::Instant;

fn create_array(size: usize) -> Vec<usize> {
    let mut arr = Vec::with_capacity(size);
    for i in 0..size {
        arr.push(i * i);
    }
    arr
}

fn main() {
    const NUM_ARRAYS: usize = 1000000;
    const ARRAY_SIZE: usize = 10;

    let start = Instant::now();

    for _ in 0..NUM_ARRAYS {
        let _my_array = create_array(ARRAY_SIZE);
    }

    let duration = start.elapsed();

    println!("Rust Time taken: {:.6} seconds", duration.as_secs_f64());
}
