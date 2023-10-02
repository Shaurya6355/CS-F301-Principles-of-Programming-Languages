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

## List Indexing (!!)
In Haskell, the !! operator is used for list indexing. It allows you to access elements at specific positions in a list

```Haskell
myList :: [Int]
myList = [1, 2, 3, 4, 5]

elementAtIndex2 :: Int
elementAtIndex2 = myList !! 2 -- This will be 3, as it's the element at index 2.

```

Keep in mind that (!!) is a partial function, meaning that it assumes the index you provide is within the bounds of the list. If you attempt to access an index that is out of bounds (e.g., an index greater than or equal to the length of the list), it will result in a runtime error. 

## Drop Keyword
In Haskell, the drop function is used to remove a specified number of elements from the beginning of a list and return the remaining elements as a new list.

```Haskell
-- Drop the first 3 elements from a list
result = drop 3 [1, 2, 3, 4, 5] -- Result: [4, 5]
```

## Length keyword
In Haskell, the length function is used to determine the number of elements in a list. 

```Haskell
-- Count the number of elements in a list
let myList = [1, 2, 3, 4, 5]
count = length myList -- Result: 5

```


## Wildcard
In Haskell, the underscore character _ is often used as a wildcard or placeholder for values that you don't intend to use or names that you want to ignore. 
```Haskell
-- A function that returns 42 regardless of its argument
always42 :: a -> Int
always42 _ = 42
```
In this example, _ is used as a placeholder for an argument that is ignored. The function always42 always returns 42, regardless of the type or value of the input argument.

## Head 
The head function is used to extract the first element of a list.

```Haskell
let myList = [1, 2, 3, 4, 5]
let firstElement = head myList -- firstElement will be 1
```

Keep in mind that head operates on non-empty lists, so it's essential to check for empty lists or use it in a situations where you can guarantee that the list is not empty.

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


## Recursive Code Snippet

```Haskell
-- Define a function to calculate the length of a list recursively
listLength :: [Int] -> Int
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

## : List Operator
The : operator in Haskell is used to construct a new list by adding an element to the front (head) of an existing list. This operation is often called "cons" and is a fundamental operation for building and manipulating lists in Haskell.

Here's how the : operator works:
- It takes an element (e.g., an integer, character, etc.) on the left and a list on the right.
- It creates a new list with the element added to the front of the existing list.
- The result is a new list that includes the element followed by the original list's elements.

```haskell
module Main where

main :: IO ()
main = do
    -- Create a list by consing elements
    let list1 = 1 : 2 : 3 : []  -- Equivalent to [1, 2, 3]
    
    -- Add an element to an existing list
    let list2 = 0 : list1     -- Adds 0 to the front of list1
    
    -- Display the lists
    putStrLn "list1:"
    print list1
    
    putStrLn "list2:"
    print list2

```
We create a list list1 using the : operator. We start with an empty list ([]) and use : to add elements to it. So, 1 : 2 : 3 : [] is equivalent to [1, 2, 3] .

## ++ Operator
++ is the list concatenation operator in Haskell. It is used to concatenate two lists together.

## Guard Operator |
```Haskell
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"
```
In this example, the grade function takes an integer score as input and uses guards to determine the corresponding letter grade based on the value of score. Depending on the value of score, different conditions are checked, and the appropriate letter grade is returned as the result. If none of the conditions match (otherwise), an "F" is returned as the default result.

