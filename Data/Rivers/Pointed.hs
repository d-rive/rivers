{-# LANGUAGE UndecidableInstances #-}

module Pointed where

data Copointed f a = At { lab :: a, root :: f a }
data Gopointed g a = Var a | Con (g a)


instance (Functor f) => Functor (Copointed f) where    fmap f (At a x) = At  (f a) (fmap f x)
instance (Functor g) => Functor (Gopointed g) where    fmap g (Var  x) = Var (g x)
                                                       fmap g (Con  x) = Con       (fmap g x)

(/\) :: (a -> b) -> (a -> f b) -> (a -> Copointed f b)
(\/) :: (a -> b) -> (g a -> b) -> (Gopointed g a -> b)

(/\) f g x       = At   (f x) (g x)
(\/) f _ (Var x) =       f x
(\/) _ g (Con x) =             g x


coup      :: (c -> b c) -> (c -> Copointed b c)
goup      :: (s a -> a) -> (Gopointed s a -> a)
down      :: (c -> Copointed b c) -> (c -> b c)
godown    :: (Gopointed s a -> a) -> (s a -> a)

coup a = id /\ a
goup a = id \/ a

down   a  = root . a
godown a  =    a . Con

                           
