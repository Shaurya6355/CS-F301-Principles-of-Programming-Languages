# Tutorial - 1

## Welcome to the First Tutorial for CS F301!
In this Tutorial we are going to explore the differences between C++ and Rust in terms of Mutability and Lifetime Concepts.

## Introduction
Programming languages play a vital role in how we build software, and understanding how they handle memory and ownership is essential for writing safe and efficient code. Let's dive into the differences between C++ and Rust in this regard.


![C++ Vs RUST](https://miro.medium.com/v2/resize:fit:1400/1*b0fcLHA-bIjxmhO4hSrPRA.jpeg)

### What is the use of mutability?
Imagine you have a box. Inside the box, you put a number. In Rust, when you make the box mutable, it means you can open the box and change the number whenever you want. Other parts of your program can also open the box and change the number. This can be powerful, but it's important to be careful. If many parts of your program can change the number at the same time, things might get confusing or even go wrong. So, while mutable variables give you flexibility, you need to manage them properly to avoid chaos.

### What is Lifetime concept in RUST?
In Rust, lifetimes help the compiler understand how long references to data are valid. They ensure that references don't outlive the data they point to, preventing dangling references and memory safety issues.

```RUST
fn print_data(data: &str) {
    println!("{}", data);
} // 'data' goes out of scope here

```
In this example, the data reference lives only within the scope of the function call. It would be incorrect to try to return this reference because the data it points to would become invalid as soon as the function ends.

**Specifying Lifetime**
```RUST
fn longest<'a>(s1: &'a str, s2: &'a str) -> &'a str {
    if s1.len() > s2.len() {
        s1
    } else {
        s2
    }
}
```
In this function, the 'a notation indicates that the references s1 and s2 must have the same lifetime 'a. This ensures that the returned reference will be valid for the same duration as the input references.

In summary, lifetimes are Rust's way of managing references to data, allowing the compiler to verify that references remain valid and preventing common memory-related errors. They're crucial for Rust's memory safety guarantees and its ability to provide high performance without sacrificing safety.


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

### WHILE LOOP

```RUST
fn main() {
    let mut count = 1; // Start from 1

    while count <= 5 {
        println!("Count: {}", count);
        count += 1; // Increment count by 1
    }
}

```
The above snippet demonstrates a **while loop** for printing numbers from 1 to 5.
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

### Merge Function in Merge Sort
The logic behind the merge function can be understood using the example of 2 sorted piles of cards

1. You compare the top cards of both piles.
2. You pick the card with the smaller number and put it into a new pile.
3. Then, you look at the next card in the pile where you took the card from and compare it again with the card in the other pile.
4. You keep doing this until you've taken all the cards from both piles and put them into the new pile in the right order.

In this way, you're merging the two sorted piles into one bigger sorted pile. It's like sorting a big deck of cards by gradually combining smaller sorted piles.

