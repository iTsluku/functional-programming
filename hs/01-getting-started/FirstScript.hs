module FirstScript where  -- one module per file

--  import Module1
--  import Module2

-- comment that occupies the part of the line to the right of the symbol

{-
nested comment (arbitrary length

GHCi cmds:
:l      load module
:r      repeat last :l cmd
:t exp  give type of expression
:i name give information about name
:q      quit
:h      help
:e file edit file

:set editor vi      set the editor to be vi
lets s = exp        give s the value of exp within this session (temporary)

ctrl+L == :!clear   clear

:browse Prelude     list of all the functions in the Prelude.hs module

-}

--  The value life is an Integer, defined to be 42.

life :: Integer
life = 42

--  The function to square an integer.

square :: Integer -> Integer
square n = n*n

--  The function to double an Integer

double :: Integer -> Integer
double n = 2*n

--  An example using double, square and life

example :: Integer
example = double (life - square (2+2))

--  functionial composition

example2 :: [Integer]
example2 = filter (not . even) [1..10] --   odd integers from 1 to 10


