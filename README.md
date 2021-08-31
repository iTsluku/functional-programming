# functional languages

## WHY
+ build secure, error-free
+ ideal toolkit for Domain Specific Languages (DSLs)
+ different perspective on programming -> knowledge
+ financial sector, web, efficient multicore programming

## WHAT
+ higher-level view of programming
+ focus on values
+ variety of features -> librarys with functions
+ powerful, elegant

# Haskell
1980s (first defined in 1990)  
named after Haskell B. Curry - pioneer of the lambda calculus  
general-purpose programming language  
GHCi (Glasgow Haskell Compiler interactive) -- interpreter

## key aspects
+ rich collection of data types
+ substantial libraries of built-in functions
+ no side-effects
+ no states
+ variables don't vary
+ I/O, work with files, interoperate with other programming languages (using monads)
+ easy to parallelize and run efficient on multicore hardware (because no state to share)
+ definitions can be used to write proofs of properties (-> validate)
+ easier to refactor
+ property-based testing (QuickCheck)
+ practical (approach of choice)
+ embedded DSLs

### functions
input (arguments/ parameters) -> output  
operate over particular types  
describe how particular data values are related  
used in writing expressions to be evaluated by the implementation  
(~rule: verbs)

### types
collection of values/ objects  
model/represent objects in specific problem domains  
~rule: Nouns)

### definitions
name :: type  
name = expression

### function composition
f1 . f2 -- output of f2 becomes the input of f1  
combine functions (-> create new function)


## coding conventions
+ names for functions begin with a small letter
+ type names begin with a capital letter
