-- | This module contains an encoding of the Klein-4 group as a deeply embedded
--   DSL in Haskell. The Klein-4 group is the group with the following
--   presentation: @< a, b : aa = bb = abab = e >@
--
module Klein4 where

-- | Group elements.
--
data Elem = E     -- identity
          | A | B -- generators
          | AB    -- other element
  deriving (Show, Eq)

-- | Abstract syntax for expressions.
--
data Expr = Lit Elem       -- literal element
          | Comp Expr Expr -- composition
  deriving (Show, Eq)

-- | Evaluation (or semantic) function for expressions.
--
eval :: Expr -> Elem
eval (Lit e)    = e
eval (Comp l r) = comp (eval l) (eval r)

-- | Law of composition.
--
comp :: Elem -> Elem -> Elem
-- E is identity
comp E x   = x
-- presentation relations
comp A A   = E
comp B B   = E
comp AB AB = E
-- definition of other element
comp A B   = AB 
-- derived rules
comp A AB  = comp (comp A A) B
comp B AB  = comp A (comp B B)
-- abelian
comp x y   = comp y x
