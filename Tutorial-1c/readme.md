# Tutorial - 1

## Welcome to the First Tutorial for CS F301!
In this Tutorial we are going to explore the differences between C++ and Rust in terms of the **Traits** concept.

## Introduction
In the world of programming, traits are powerful abstractions that enable developers to define and enforce specific behaviors or capabilities for types, promoting code reusability and maintainability. Although the concept of traits exists in various programming languages, this tutorial will focus on how traits are implemented in two distinct languages: C++ and Rust.


![C++ Vs RUST](https://miro.medium.com/v2/resize:fit:1400/1*b0fcLHA-bIjxmhO4hSrPRA.jpeg)

### What is Traits?
In programming, think of traits as special skills or superpowers that we can give to different types of data. Just like superheroes have unique abilities, types in programming have their own characteristics. Traits allow us to define sets of skills that types can learn.

For instance, imagine we create a "Swimmable" trait that describes the ability to swim and a "PlayableInstrument" trait for playing musical instruments. We can then make various types like "Fish" and "Guitar" learn these traits. So, our "Fish" type can swim, and our "Guitar" type can be played like a musical instrument.

Traits are incredibly useful because they promote code reusability and organization. Instead of writing the same code for different types, we define traits once and let multiple types learn them. This approach makes our code more versatile and efficient, like creating unique characters in a game by giving them different abilities. Traits are a powerful tool for making our code flexible and organized.

## How to use traits in RUST?

### Defining a trait

Let's create a simple trait called Drawable that defines a method "draw".

```RUST
// Define a trait named 'Drawable' with a 'draw' method.
trait Drawable {
    fn draw(&self);
}
```
### Implementing a trait

Now, let's create a struct and implement the Drawable trait for it. We'll create a Circle struct and give it the ability to be drawn.

```RUST
// Implement the 'Drawable' trait for 'Circle'.
struct Circle {
    radius: f64,
}

impl Drawable for Circle {
    fn draw(&self) {
        println!("Drawing a circle with radius {}", self.radius);
    }
}

```

### Using the trait
Now, let's use our Circle struct and the Drawable trait to draw a circle.
```RUST
fn main() {
    let circle = Circle { radius: 3.0 };
    circle.draw();
}
```
In the main function, we create a Circle instance and call the draw method on it. This will print "Drawing a circle with radius 3.0" to the console.

### Polymorphism with Traits
One of the powerful features of traits is polymorphism. We can create different types that implement the same trait and use them interchangeably. Let's create another type, Rectangle, and make it drawable as well.

```RUST
struct Rectangle {
    width: u32,
    height: u32,
}

impl Drawable for Rectangle {
    fn draw(&self) {
        println!("Drawing a rectangle with dimensions {} x {}", self.width, self.height);
    }
}

```
Now, we can create both circles and rectangles and draw them using the Drawable trait.

```RUST
fn main() {
    let circle = Circle { radius: 3.0 };
    let rectangle = Rectangle { width: 5, height: 4 };

    let shapes: Vec<Box<dyn Drawable>> = vec![
        Box::new(circle),
        Box::new(rectangle),
    ];

    for shape in shapes {
        shape.draw();
    }
}
```
In this code, we create a vector of shapes that implement the Drawable trait and then iterate through them, calling the draw method. This demonstrates how we can use traits for polymorphism and handle different types in a unified way.


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




