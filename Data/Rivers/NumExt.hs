{-# LANGUAGE FlexibleInstances #-}

-- | Adds a few useful operators/functions to |Num|.

module Data.Rivers.NumExt (
    module Data.Rivers.NumExt,
    module Data.Ratio
 ) where

import Prelude (Eq(..), Ord(..), Num(..), Integral(..), Integer, error, otherwise)
import qualified Prelude
import Data.Ratio

infixl 7 /
infixr 8 ^

class (Num a, Ord a) => NumExt a where
   (/), (^)     :: a -> a -> a  -- NB. we include '/' to be able to define 'choose' uniformly
   fact         :: a -> a
   fall, choose :: a -> a -> a

   -- | Factorials.
   fact 0 =  1
   fact n =  n * fact (n - 1)

   -- | Falling factorial powers (see CMath, p.47).
   fall _ 0  =  1
   fall x n  =  x * fall (x - 1) (n - 1)

   -- | Binomial coefficients.

   choose x k
        | k < 0      =  0
        | otherwise  =  fall x k / fact k  -- TODO: improve

instance NumExt Integer where
  (^) = (Prelude.^)
  (/) = div

instance (NumExt a, Integral a) => NumExt (Ratio a) where
   m ^ n = if denominator n == 1
           then (numerator m ^ numerator n) % (denominator m ^ numerator n)
           else error "^: Ratio"
   (/) = (Prelude./)

