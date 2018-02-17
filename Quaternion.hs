-- | This module contains an encoding of the Quaternion group as a deeply
--   embedded DSL in Haskell. The Quaternion group is the group with the
--   following presentation: @< f, i, j, k : ff = e, ii = jj = kk = ijk = f >@
--
module Quaternion where

-- | Group elements.
--
data Elem = E             -- identity
          | F | I | J | K -- generators
          | FI | FJ | FK  -- other elements
  deriving (Show, Eq)

-- | Abstract syntax for expressions.
--
data Expr = Lit Elem       -- literal element
          | Comp Expr Expr -- composition
  deriving (Show, Eq)

-- | Evaluation function for expressions.
--
eval :: Expr -> Elem
eval (Lit e)    = e
eval (Comp l r) = comp (eval l) (eval r)

-- | Law of composition.
--
comp :: Elem -> Elem -> Elem
-- E is identity
comp E x  = x
comp x E  = x
-- presentation relations
comp F F  = E
comp I I  = F
comp J J  = F
comp K K  = F
-- definitions for other elements
comp F I  = FI
comp F J  = FJ
comp F K  = FK
-- derived from definitions
comp F FI = comp (comp F F) I
comp F FJ = comp (comp F F) J
comp F FK = comp (comp F F) K
-- f is in center (derived from ff = e)
comp x F  = comp F x
-- derived from ijk = e
comp I J  = K
comp I K  = FJ
comp J I  = FK
comp J K  = I
comp K I  = J
comp K J  = FI
-- other derived rules
comp I FI = comp F (comp I I) -- i and fi are inverses
comp I FJ = comp F (comp I J)
comp I FK = comp F (comp I K)
comp J FI = comp F (comp J I)
comp J FJ = comp F (comp J J) -- j and fj are inverses
comp J FK = comp F (comp J K)
comp K FI = comp F (comp K I)
comp K FJ = comp F (comp K J)
comp K FK = comp F (comp K K) -- k and fk are inverses
-- at this point if x is a generator, then comp x is total
comp FI x = comp F (comp I x)
comp FJ x = comp F (comp J x)
comp FK x = comp F (comp K x)
