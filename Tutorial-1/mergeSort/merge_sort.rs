use std::time::Instant;

fn create_array(size: usize) -> Vec<usize> {
    let mut arr = Vec::with_capacity(size);
    for i in 0..size {
        arr.push(i * i);
    }
    arr
}

fn merge(arr: &mut [usize], left: usize, mid: usize, right: usize) {
    let n1 = mid - left + 1;
    let n2 = right - mid;

    let mut L = Vec::with_capacity(n1);
    let mut R = Vec::with_capacity(n2);

    for i in 0..n1 {
        L.push(arr[left + i]);
    }
    for j in 0..n2 {
        R.push(arr[mid + 1 + j]);
    }

    let mut i = 0;
    let mut j = 0;
    let mut k = left;

    while i < n1 && j < n2 {
        if L[i] <= R[j] {
            arr[k] = L[i];
            i += 1;
        } else {
            arr[k] = R[j];
            j += 1;
        }
        k += 1;
    }

    while i < n1 {
        arr[k] = L[i];
        i += 1;
        k += 1;
    }

    while j < n2 {
        arr[k] = R[j];
        j += 1;
        k += 1;
    }
}

fn merge_sort_helper(arr: &mut [usize], left: usize, right: usize) {
    if left < right {
        let mid = left + (right - left) / 2;
        merge_sort_helper(arr, left, mid);
        merge_sort_helper(arr, mid + 1, right);
        merge(arr, left, mid, right);
    }
}

fn merge_sort(arr: &mut [usize]) {
    let len = arr.len();
    if len > 1 {
        merge_sort_helper(arr, 0, len - 1);
    }
}

fn main() {
    const START_SIZE: usize = 10000;
    const END_SIZE: usize = 100000;
    const STEP_SIZE: usize = 10000;

    for list_size in (START_SIZE..=END_SIZE).step_by(STEP_SIZE) {
        let mut numbers = create_array(list_size);

        let start = Instant::now();

        merge_sort(&mut numbers);

        let duration = start.elapsed();

        println!("Rust List Size: {}, Time taken: {:.6} seconds", list_size, duration.as_secs_f64());
    }
}
