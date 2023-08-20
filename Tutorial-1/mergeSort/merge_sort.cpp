#include <iostream>
#include <chrono>
#include <vector>
#include <algorithm>

std::vector<int> create_array(int size) {
    std::vector<int> arr;
    arr.reserve(size);
    for (int i = 0; i < size; ++i) {
        arr.push_back(i * i);
    }
    return arr;
}

void merge(std::vector<int>& arr, int left, int mid, int right) {
    
    //WRITE YOUR CODE HERE

}

void merge_sort_helper(std::vector<int>& arr, int left, int right) {
    
    //WRITE YOUR CODE HERE


}

void merge_sort(std::vector<int>& arr) {
    int len = arr.size();
    if (len > 1) {
        merge_sort_helper(arr, 0, len - 1);
    }
}

int main() {
    const int START_SIZE = 10000;
    const int END_SIZE = 100000;
    const int STEP_SIZE = 10000;

    for (int list_size = START_SIZE; list_size <= END_SIZE; list_size += STEP_SIZE) {
        std::vector<int> numbers = create_array(list_size);

        auto start = std::chrono::high_resolution_clock::now();

        merge_sort(numbers);

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> duration = end - start;

        std::cout << "C++ List Size: " << list_size << ", Time taken: " << duration.count() << " seconds" << std::endl;
    }

    return 0;
}

