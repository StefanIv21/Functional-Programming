# Functional-Programming

Used functional programming languages such as Haskell, Racket, and Prolog in order to understand the functional programming concepts, immutability, higher-order functions, pure functions, recursion, declarative programming, and advanced type systems

# Racket-SMP
- In SMP, an equal number of men and women are given and, for each of them, a "ranking" of all persons of the opposite sex, and each man is required to marry one woman so that there is not one man and one woman. woman in different marriages who would prefer each other over their husbands. If such a pair existed, the two would be motivated to leave their partners to be together, so their marriages would not be stable (hence the name of the problem)

- An SMP instance in Racket is seen as two lists (of equal length) of male and female preferences, respectively.Thus, the male/female preference list is a list of lists, with each inner list having a male/female in the first position, and the following positions people of the opposite sex in the order of preferences of this man/woman
## solving the problem of stable marriages using the Gale Shapley algorithm and functions for manipulating lists and streams

# Haskell- Functional data structures
- Haskell implementation of a priority queue using binomial heaps. The standard imperative implementation of a heap uses vectors and relies on constant-time random access. As we know, linked lists in functional languages do not lend themselves to a linear-time random access approach. Therefore, we will use an alternative representation, in the form of lists of binomial trees, which provides logarithmic complexity for all operations, including that of combining two heaps, which in the standard imperative approach is performed in linear time.

# Prolog - Carcassonne game

## implementation of several mechanisms from the mechanics of the Carcassonne board game such as:
- how we can represent Carcassonne pieces and later a game board
- how we can match the pieces and how we can find a position where a piece fits into play
- how we can  calculate a player's points

## Purpose
- Application of logic programming in the Prolog language.
- Using mechanisms to automatically find all solutions for a goal.
