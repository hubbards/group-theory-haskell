{-# LANGUAGE FlexibleInstances #-}

-- | This module contains a type classes for monoids and groups.
--
--   A monoid is a set @a@ and binary operator @mop@ where the following
--   properties hold:
--   1. @mop@ is associative
--   2. @a@ has an identity element @mid@
--
--   A group is a set @a@ and binary operator @gop@ where the following
--   properties hold:
--   1. @a@ and @gop@ are a monoid
--   2. every element @x@ of @a@ has an inverse @ginv x@
--
module Group where

import Prelude hiding (Monoid(..))

-- -----------------------------------------------------------------------------
-- Type classes

-- | Type class for monoids. 
--
--   Monoid laws:
--
--   prop> x `mop` y `mop` z = x `mop` (y `mop` z)
--
--   prop> mid `mop` x = x
--
--   prop> x `mop` mid = x
--
class Monoid a where
  -- Identity element
  mid :: a
  -- Law of composition
  mop :: a -> a -> a

-- | Type class for groups. 
--
--   Group laws:
--
--   prop> gid = mid
--
--   prop> gop = mop
--
--   prop> ginv x `gop` x = gid
--
--   prop> x `gop` ginv x = gid
--
class Monoid a => Group a where
  -- Identity element
  gid :: a
  gid = mid
  -- Law of composition
  gop :: a -> a -> a
  gop = mop
  -- Inverses
  ginv :: a -> a

-- -----------------------------------------------------------------------------
-- Examples

-- | Integers are a monoid under addition.
--
instance Monoid Int where
  mid = 0
  mop = (+)

-- | Integers are a group under addition.
--
instance Group Int where
  ginv = negate

-- | Booleans are a monoid under symmetric difference.
--
instance Monoid Bool where
  mid = False
  x `mop` y = x && not y || y && not x

-- | Booleans are a group under symmetric difference.
--
instance Group Bool where
  ginv = not

-- | Lists are a monoid under concatination.
--
instance Monoid [a] where
  mid = []
  mop = (++)

-- | Functions (with same domain and codomain) are a monoid under composition.
--
instance Monoid (a -> a) where
  mid = id
  mop = (.)
