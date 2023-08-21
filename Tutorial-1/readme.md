# Tutorial - 1

## Welcome to first Tutorial for CS F301!
In this Tutorial we are going to explore the differences between C++ and Rust in terms of Ownership and Memory Allocation Concepts.

## Introduction
Programming languages play a vital role in how we build software, and understanding how they handle memory and ownership is essential for writing safe and efficient code. Let's dive into the differences between C++ and Rust in this regard.


![C++ Vs RUST](https://miro.medium.com/v2/resize:fit:1400/1*b0fcLHA-bIjxmhO4hSrPRA.jpeg)

### What is ownership?
Imagine you have a pet rock named Rocky. Rocky is your responsibility; you have to take care of it, play with it, and keep it safe.

In programming, ownership is a bit like that. When you create a piece of data, like a number or some text, it's your responsibility to manage it properly. Just like with Rocky, you need to make sure you use it correctly, don't lose track of it, and don't let others mess with it when you're not looking.

In some languages, like C++, you have to do everything for your data – you need to create space for it, clean up after it, and make sure it's not used by mistake. It's like looking after Rocky without any help. If you forget to clean up, your room gets cluttered with rocks!

But in Rust, it's a bit different. Rust helps you take care of your data. It watches over you like a helper robot. When you're done with your data, Rust makes sure it's put away safely. And if someone tries to use your data when they shouldn't, Rust steps in and says, "Hey, that's not safe!"

So, ownership in programming is about taking care of your data. Rust helps you do it easily, like having a little robot friend to watch over your digital stuff and make sure everything is tidy and safe.

### What is Memory Allocation?
Memory allocation is like getting a shelf (memory) to store your things (data). In C++, you decide how many shelves you need and where to put them. Mistakes can lead to a messy room (bugs). In Rust, a helper sets up the shelves just right and cleans up afterward. It prevents messy rooms (memory leaks) and makes sure nobody messes with your things unexpectedly (safe memory use). It's like having a smart shelf organizer in your programming toolbox.

## Implementation
This Next section is going to provide you with some basic starter code that will help you with todays tutorial. 
**DO NOT WORRY if you have not used RUST before**
The tutorial only asks you to write some very basic code and we have provided some code in the following sections to help you out.

## RUST

### Anatomy of a RUST Program

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

The first code snippet will give a compile time error. This is because variable in RUST are by default immutable. This means you cannot reassign any values to the same variable. however you can change this behavior of the variables by using the  mut keyword, as described by the second code snippet.
## C++



