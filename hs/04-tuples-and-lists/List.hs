module List where

{-
combine an arbitrary number of values (same type) into a single object.

same type -> allows type check prior to execution -> type errors will be found before a program is actually executed.


-}
type ShopItem = (String, Int)

type Basket = [ShopItem]
--  [("Salt: 1kg",69),("plain crisps",25)] :: Basket
--  type String = [Char]    --  special case of list type (synonyms fot the type which it names -- like ShopItem)
--  type introduces a definition of a type, not a value
--  type names begin with capital letters
