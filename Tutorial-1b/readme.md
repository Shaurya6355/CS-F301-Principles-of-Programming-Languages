# Tutorial - 1

## Welcome to the First Tutorial for CS F301!
In this Tutorial we are going to explore the differences between C++ and Rust in terms of Ownership and Memory Allocation Concepts.

## Introduction
Programming languages play a vital role in how we build software, and understanding how they handle memory and ownership is essential for writing safe and efficient code. Let's dive into the differences between C++ and Rust in this regard.


![C++ Vs RUST](https://miro.medium.com/v2/resize:fit:1400/1*b0fcLHA-bIjxmhO4hSrPRA.jpeg)

### What is the use of mutability?
Imagine you have a box. Inside the box, you put a number. In Rust, when you make the box mutable, it means you can open the box and change the number whenever you want. Other parts of your program can also open the box and change the number. This can be powerful, but it's important to be careful. If many parts of your program can change the number at the same time, things might get confusing or even go wrong. So, while mutable variables give you flexibility, you need to manage them properly to avoid chaos.

### What is Lifetime concept in RUST?
In Rust, lifetimes help the compiler understand how long references to data are valid. They ensure that references don't outlive the data they point to, preventing dangling references and memory safety issues.

```RUST
fn main() {
    let value = 42; // A variable named 'value'

    let result; // A variable to hold the result of the reference
    {
        let reference = &value; // A reference to 'value'
        result = add_one(reference); // Pass the reference to the function
    } // 'reference' goes out of scope here, but 'result' still lives

    println!("Result: {}", result); // 'result' is still valid here
}

fn add_one(input: &i32) -> i32 {
    input + 1 // Return the incremented value
}

```

The lifetime of reference is limited by the inner block. It cannot be used outside of that block because it would refer to data that is no longer valid. This restriction helps avoid referencing data that has already been deallocated.
By using lifetimes, Rust ensures that references are used safely and discarded when they should be. It's like having a conversation with the compiler to ensure that borrowed data is treated responsibly and doesn't outlive its usefulness.

## Implementation
This Next section is going to provide you with some basic starter code that will help you with todays tutorial. 
**DO NOT WORRY if you have not used RUST before**
The tutorial only asks you to write some very basic code and we have provided some code in the following sections to help you out.

## RUST

### Anatomy of a RUST Program

```RUST
fn function_name(parameters: Type) -> Return_Type {
    // Function body
    // ...
    // Optional: Return a value using the `return` keyword
}
```

```RUST
fn main()
{
  println!("Hello World");
}
```

The main function is special: it is always the first code that runs in every executable Rust program. It is also important to note that println! is a RUST macro and not a function.Remember to end you statemnets with a semi colon. 

### Variables and Mutability
```RUST
fn main() {
    let x = 5;
    println!("The value of x is: {x}");
    x = 6;
    println!("The value of x is: {x}");
}
```

```RUST
fn main() {
    let mut x = 5;
    println!("The value of x is: {x}");
    x = 6;
    println!("The value of x is: {x}");
}
```

The first code snippet will give a compile time error. This is because variable in RUST are by default immutable. This means you cannot reassign any values to the same variable. However you can change this behavior of the variables by using the  mut keyword, as described by the second code snippet.

### Creating Vectors

```RUST
let mut my_vector: Vec<i32> = Vec::with_capacity(5);
```

The above code snipped defines a vector of datatype Vec<i32>, which means it's a vector that holds 32-bit signed integers (i32). The with_capacity(size) function creates a new vector with an initial capacity specified by size.

```RUST
my_vector.push(element);
```
The push() function is used to add elements to the vector. 

### FOR Loop

```RUST
for variable in start..end {
    // Loop body
}
```

The **variable** represents the current value of the loop as you iterate through the range.
start(inclusive) and end(exclusive) represent the range for the loop.

```RUST
fn main() {
    for number in 1..6 {
        println!("The number is: {}", number);
    }
}
```
The above snippet demonstrates a **for loop** for printing numbers from 1 to 5.

### Printing
```RUST
 println!("{:?}", my_vector);
```
Use the above code for printing a vector.
This is the formatting placeholder that tells Rust to print the value using its "Debug" format. The :? indicates that Rust should use the Debug trait implementation for formatting the value.
## C++

### Vector
```C++
#include <iostream>
#include <vector>

int main() {
    // Declare a vector to store strings
    std::vector<std::string> names;

    // Add elements to the vector using push_back
    names.push_back("Alice");
    names.push_back("Bob");
    names.push_back("Charlie");

    // Access elements by index
    std::cout << "Second name: " << names[1] << std::endl;

    // Get the size of the vector
    std::cout << "Vector size: " << names.size() << std::endl;

    // Iterate through the vector using an iterator
    std::cout << "Names: ";
    for (const std::string &name : names) {
        std::cout << name << " ";
    }
    std::cout << std::endl;

    // Clear the vector
    names.clear();

    // Check if the vector is empty
    if (names.empty()) {
        std::cout << "Vector is empty." << std::endl;
    }

    return 0;
}

```
Example code for using the Vector STL in C++



