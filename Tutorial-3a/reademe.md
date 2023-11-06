# Tutorial - 3

## Welcome to the Third Tutorial for CS F301!
In this Tutorial we are going to explore the basic concepts about Figaro.

![Figaro](https://opengraph.githubassets.com/c886b8202492bf80dfc0626edd1c3cd18a8cc76483f4853125788de4692f1308/charles-river-analytics/figaro)

## Introduction
Figaro is a probabilistic programming framework for the Scala programming language. It's designed to simplify the development of probabilistic models and to perform probabilistic inference in a straightforward and efficient manner. Figaro facilitates this by allowing developers to create and manipulate probabilistic models in a declarative way.

## Implementation

### Defining Random Variables
In Figaro, you define random variables using elements like Constant, Uniform, Select, Flip, etc. These elements represent probabilistic variables with specific probability distributions.

Flip is one of the simplest elements in Figaro, representing a Boolean random variable. It models a binary outcome, such as a coin flip, where the probability of the outcome being true (e.g., heads) is specified by the parameter p.
```Figaro
import com.cra.figaro.language._

// Define a random variable representing a coin flip
val coinFlip = Flip(0.5)
//In this example, coinFlip models a fair coin flip with a 50% probability of being true (heads) and a 50% probability of being false (tails).
```

Select is used to create a random choice among a set of options, each associated with a specified probability. It's like a weighted coin flip with multiple outcomes.
```Figaro
// Define a random variable for selecting a color
val color = Select(0.2 -> "Red", 0.5 -> "Green", 0.3 -> "Blue")

//In this example, color represents a random choice among "Red," "Green," and "Blue" with the given probabilities.
```

Constant represents a deterministic, non-random variable. It's used to model known, fixed values or constants in your probabilistic model.
```Figaro
// Define a constant representing the acceleration due to gravity
val gravity = Constant(9.81)

```

The Uniform element in Figaro represents a probabilistic choice among a set of options, where each option is equally likely to be chosen. It is often used when you have a collection of values, and you want to model a random choice from that collection with uniform probabilities.
```Figaro
// Define a random variable for selecting a card from a deck of cards (uniformly)
val deckOfCards = List("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King")
val selectedCard = Uniform(deckOfCards.map(Constant(_)): _*)

//In this example, selectedCard represents a random choice from a standard deck of cards. Each card in the deck is an option, and they all have equal probabilities of being selected.
```


