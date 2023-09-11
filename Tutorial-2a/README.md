# Tutorial - 2

## Welcome to the Second Tutorial for CS F301!
In this Tutorial we are going to explore the basic concepts about the Haskell Programing language.

![Haskell](https://miro.medium.com/v2/resize:fit:1166/1*-DMa8q1JrW7CG6imgITacA.png)

## Introduction
Haskell is a statically typed, purely functional programming language that was first developed in the late 1980s by a group of researchers led by Haskell Curry and Simon Peyton Jones. It is named after Haskell Curry, a mathematician and logician who made significant contributions to the field of combinatory logic and functional programming.

## Implementation
This Next section is going to provide you with some basic starter code that will help you with todays tutorial. 
**DO NOT WORRY if you have not used Haskell before**
The tutorial only asks you to write some very basic code and we have provided some code in the following sections to help you out.

## Main Function

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

## NOTE: To calculate mod in haskell, use the following syntax
```Haskell
x `mod` y
```

## User Defined Functions
Functions in Haskell follow the below syntax

```Haskell
functionName :: TypeSignature
functionName parameter1 parameter2 ... = functionBody
```

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
- Functions are defined using the pattern functionName arguments = ...

## Recursion

```haskell
-- Factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
    let n = 5
    putStrLn ("The factorial of " ++ show n ++ " is " ++ show (factorial n))
```

Simple Code snippet to explain how recursion is implemented in Haskell.

## Function Composition
Function composition in Haskell is a powerful technique for combining two or more functions to create a new function. It allows you to chain functions together, passing the output of one function as the input to another. The syntax for function composition in Haskell uses the . (dot) operator. Here's how it works:

Suppose you have two functions:

f :: A -> B

g :: B -> C

You can create a new function h by composing f and g such that h takes an argument of type A, applies f to it, and then applies g to the result to produce a value of type C. The syntax for function composition is as follows:

```haskell
h = g . f
```
Here's a breakdown of the syntax:

. (dot) is the function composition operator.
g and f are the functions being composed.
h is the resulting composed function.
In this composition, f is applied first, followed by g. When you call h x, it is equivalent to g (f x). The output of f x becomes the input to g.

**Note- sum is also a Haskell function so try to directly use it in your implementation** 

## Recursive Code Snippet

```Haskell
-- Define a function to calculate the length of a list recursively
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs
```

- :: [Int] -> Int specifies the type signature of the function.
    - It indicates that sumList takes a list of integers ([Int]) as input and returns an integer (Int) as the result.
- The second line is a pattern matching clause that handles the case when the input list is empty ([]). It serves the purpose of a base case.
- This clause handles the case when the input list is not empty and consists of at least one element.
    - The pattern (x:xs) is used to destructure the list. It matches the head of the list (x) and the rest of the list (xs).
    - x represents the first element of the list.
    - xs represents the remaining elements of the list (it's a list itself).

## EXAMPLE PROGRAM
```
-- Define a function to compare two numbers and print a message using the | operator
compareNumbers :: Int -> Int -> IO ()
compareNumbers x y
    | x < y     = putStrLn ("The first number (" ++ show x ++ ") is less than the second number (" ++ show y ++ ").")
    | x > y     = putStrLn ("The first number (" ++ show x ++ ") is greater than the second number (" ++ show y ++ ").")
    | x == y    = putStrLn ("The first number (" ++ show x ++ ") is equal to the second number (" ++ show y ++ ").")
    | otherwise = putStrLn "Invalid input."
```

- :: Int -> Int -> IO() specifies the type signature of the function. This function takes two integer inputs and returns a string to the output console.
