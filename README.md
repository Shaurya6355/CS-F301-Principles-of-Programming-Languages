# CS-F301-Principles-of-Programming-Languages
Welcome to CS F301. This repository will contain all the tutorials for this course.

What is ownership?

Ownership is a set of rules that govern how a Rust program manages memory. All programs must manage how they use a computer’s memory while running. Some languages have garbage collection that regularly looks for no longer-used memory as the program runs; in other languages, the programmer must explicitly allocate and free the memory.Rust uses an approach where memory is managed through a system of ownership with a set of rules that the compiler checks. If any of the rules are violated, the program won’t compile. None of the features of ownership will slow down your program while it’s running.
