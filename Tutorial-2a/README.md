# Tutorial - 2

## Welcome to the Second Tutorial for CS F301!
In this Tutorial we are going to 

## Introduction
Haskell is a statically typed, purely functional programming language that was first developed in the late 1980s by a group of researchers led by Haskell Curry and Simon Peyton Jones. It is named after Haskell Curry, a mathematician and logician who made significant contributions to the field of combinatory logic and functional programming.

## Implementation
This Next section is going to provide you with some basic starter code that will help you with todays tutorial. 
**DO NOT WORRY if you have not used Haskell before**
The tutorial only asks you to write some very basic code and we have provided some code in the following sections to help you out.

### Main Function
```Haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```
- main is the entry point for your Haskell program.
- :: IO () specifies the type of main. It's an I/O action that performs some input/output operations.
putStrLn is a function that prints a string to the console.

```Haskell
main :: IO ()
main = do
    let x = 5
    putStrLn ("The value of x is: " ++ show x)
```
- let is used to declare a variable.
- ++ is used for string concatenation.
- 'show' converts a value to its string representation.

### User Defined Functions
```Haskell
-- Function to calculate the square of a number
square :: Int -> Int
square x = x * x

main :: IO ()
main = do
    let number = 4
    putStrLn ("The square of " ++ show number ++ " is " ++ show (square number))
```
- square is a function that takes an Int as an argument and returns an Int.
- Functions are defined using the pattern functionName arguments = ..
