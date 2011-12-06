{-# LANGUAGE UndecidableInstances #-}
module Tree where

import Prelude hiding (map, repeat, unzip, zip)

data Tree a = Node { root :: a, left :: Tree a, right :: Tree a} deriving (Read, Show)


repeat a  = t where t = Node a t t
zip g t u = Node (g (root t) (root u)) (zip g (left t) (left u)) (zip g (right t) (right u))
map f t   = Node (f (root t)) (map f (left t)) (map f (right t))
unzip t   = (map fst t, map snd t)


minDepth :: Int
minDepth = 30

equal :: (Eq a) => Int -> Tree a -> Tree a -> Bool
equal 0 t u = root t == root u
equal n t u = root t == root u 
               && equal (n-1) (left  t) (left  u)
               && equal (n-1) (right t) (right u)

instance (Eq a) => Eq (Tree a) where
     (==) t u = equal minDepth t u
instance (Ord a) => Ord (Tree a) where
     (<=) t u = error "Ord not really implemented"

instance (Num a) => Num (Tree a) where
     (+) = zip (+)
     (-) = zip (-)
     (*) = zip (*)
     negate      = map negate
     abs         = map abs
     signum      = map signum
     fromInteger = repeat . fromInteger

instance (Integral a) => Integral (Tree a) where
     div  = zip div
     mod  = zip mod
     quotRem s t = unzip (zip quotRem s t)
     toInteger t = toInteger (root t)

instance (Fractional a) => Fractional (Tree a) where
     (/) = zip (/)
     recip        = map recip
     fromRational = repeat . fromRational

instance (Enum a) => Enum (Tree a) where
     toEnum i = repeat (toEnum i)
     fromEnum t = fromEnum (root t)

instance (Real a) => Real (Tree a) where
     toRational t = toRational (root t)


