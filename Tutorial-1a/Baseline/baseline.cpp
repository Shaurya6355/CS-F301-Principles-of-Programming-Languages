#include <iostream>
#include <chrono>
#include <vector>

std::vector<size_t> create_array(size_t size) {
    std::vector<size_t> arr;
    arr.reserve(size);
    for (size_t i = 0; i < size; ++i) {
        arr.push_back(i * i);
    }
    return arr;
}

int main() {
    const size_t NUM_ARRAYS = 1000000;
    const size_t ARRAY_SIZE = 10;

    auto start = std::chrono::high_resolution_clock::now();

    for (size_t i = 0; i < NUM_ARRAYS; ++i) {
        std::vector<size_t> my_array = create_array(ARRAY_SIZE);
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration = end - start;

    std::cout << "C++ Time taken: " << duration.count() << " seconds" << std::endl;

    return 0;
}
