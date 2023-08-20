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
    int n1 = mid - left + 1;
    int n2 = right - mid;

    std::vector<int> L(n1);
    std::vector<int> R(n2);

    for (int i = 0; i < n1; ++i) {
        L[i] = arr[left + i];
    }
    for (int j = 0; j < n2; ++j) {
        R[j] = arr[mid + 1 + j];
    }

    int i = 0, j = 0, k = left;
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k++] = L[i++];
        } else {
            arr[k++] = R[j++];
        }
    }

    while (i < n1) {
        arr[k++] = L[i++];
    }
    while (j < n2) {
        arr[k++] = R[j++];
    }
}

void merge_sort_helper(std::vector<int>& arr, int left, int right) {
    if (left < right) {
        int mid = left + (right - left) / 2;
        merge_sort_helper(arr, left, mid);
        merge_sort_helper(arr, mid + 1, right);
        merge(arr, left, mid, right);
    }
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

