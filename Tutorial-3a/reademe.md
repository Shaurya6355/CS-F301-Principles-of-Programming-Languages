# Tutorial - 3

# Welcome to the Third Tutorial for CS F301!
In this Tutorial we are going to explore the basic concepts about Figaro.

<!--[Figaro](https://b3069131.smushcdn.com/3069131/wp-content/uploads/2020/12/Core_R_D_Figaro_teaser.jpg?lossy=1&strip=1&webp=1)-->

<div style="text-align:center;">
  <img src="https://b3069131.smushcdn.com/3069131/wp-content/uploads/2020/12/Core_R_D_Figaro_teaser.jpg?lossy=1&strip=1&webp=1" alt="Your Image" width="100%">
</div>

# Introduction
Figaro is a probabilistic programming framework for the Scala programming language. It's designed to simplify the development of probabilistic models and to perform probabilistic inference in a straightforward and efficient manner. Figaro facilitates this by allowing developers to create and manipulate probabilistic models in a declarative way.

# Implementation

## Defining Random Variables
In Figaro, you define random variables using elements like Constant, Uniform, Select, Flip, etc. These elements represent probabilistic variables with specific probability distributions.

## Flip 
Flip is one of the simplest elements in Figaro, representing a Boolean random variable. It models a binary outcome, such as a coin flip, where the probability of the outcome being true (e.g., heads) is specified by the parameter p.
```Scala
import com.cra.figaro.language._

// Define a random variable representing a coin flip
val coinFlip = Flip(0.5)
//In this example, coinFlip models a fair coin flip with a 50% probability of being true (heads) and a 50% probability of being false (tails).
```

## Select 
Select is used to create a random choice among a set of options, each associated with a specified probability. It's like a weighted coin flip with multiple outcomes.
```Scala
// Define a random variable for selecting a color
val color = Select(0.2 -> "Red", 0.5 -> "Green", 0.3 -> "Blue")

//In this example, color represents a random choice among "Red," "Green," and "Blue" with the given probabilities.
```

## Constant
Constant represents a deterministic, non-random variable. It's used to model known, fixed values or constants in your probabilistic model.
```Scala
// Define a constant representing the acceleration due to gravity
val gravity = Constant(9.81)

```

## Uniform
Uniform element in Figaro represents a probabilistic choice among a set of options, where each option is equally likely to be chosen. It is often used when you have a collection of values, and you want to model a random choice from that collection with uniform probabilities.
```Scala
// Define a random variable for selecting a card from a deck of cards (uniformly)
val deckOfCards = List("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King")
val selectedCard = Uniform(deckOfCards.map(Constant(_)): _*)

//In this example, selectedCard represents a random choice from a standard deck of cards. Each card in the deck is an option, and they all have equal probabilities of being selected.
```

---

# Conditional Probability
You can express conditional probability using constructs like IF, Chain, and CPD (Conditional Probability Distribution). These allow you to define dependencies between random variables.

## IF 
The If element is used to create a random variable that depends on a condition. If the condition is true, the random variable takes on the value specified in the thenBranch; otherwise, it takes on the value specified in the elseBranch.
```Scala
import com.cra.figaro.language._
import com.cra.figaro.library.atomic._

// Define a random variable for a student's grade
val passedExam = Flip(0.7)

// Define an If element to model the student's grade
val grade = If(passedExam, Constant("A"), Constant("F"))

// Observe the grade
grade.observe("A")


//In this example, the random variable grade depends on whether the student passed the exam (passedExam). If the student passed (true), the grade is "A"; otherwise, it's "F."
```

## CPD
CPD (Conditional Probability Distribution) element is used to define conditional dependencies between multiple parent random variables and a child random variable. It represents a probability distribution for the child variable based on the combinations of parent variable values.

```Scala
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

// Define random variables for the parent variables A, B, and C
val A = Flip(0.3)
val B = Select(0.2 -> 'X', 0.8 -> 'Y')
val C = Constant('Z')

// Define a CPD for a child variable D based on the values of A, B, and C
val D = CPD(A, B, C,
  (false, 'X', 'Z') -> Flip(0.1),
  (false, 'X', 'Z') -> Flip(0.9),
  (true, 'X', 'Z') -> Flip(0.4),
  (true, 'Y', 'Z') -> Flip(0.7),
  (true, 'Y', 'Z') -> Flip(0.3)
)

// Observe the result of D
D.observe(true)

```
The CPD specifies the conditional probabilities for different combinations of parent variable values and maps them to the probability of the child variable D being true. In this example, the CPD depends on the values of A, B, and C, and there are five different conditional probabilities defined based on different combinations of the parent values.
