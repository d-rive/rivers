module Data.Rivers.Idiom where

import Prelude (($))
class Idiom f where
     pure      :: a -> f a
     srepeat    :: a -> f a     
     (<>)      :: f (a -> b)       -> (f a -> f b)
     smap       ::   (a -> b)       -> (f a -> f b)
     zip       ::   (a -> b -> c)  -> (f a -> f b -> f c)

     pure      = srepeat
     (<>)      = zip ($)
     srepeat a  = pure a
     smap f s   = pure f <> s
     zip g s t = pure g <> s <> t
