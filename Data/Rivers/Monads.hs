{-# LANGUAGE UndecidableInstances #-}
module Data.Rivers.Monads where

data Cofree f a = Root { extract :: a, branch :: f (Cofree f a) }
data   Free f a = Var a | Com (f (Free f a))


instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where show (Root a x) = show a ++ "@" ++ show x
instance (Show a, Show (f (Free   f a))) => Show (  Free f a) where show (Var    x) = show x
                                                                    show (Com    x) = show x

eval' :: (Functor f) => (a -> b) -> (f b -> b) -> (Free f a -> b)
eval' var ___ (Var x) = var x
eval' var com (Com x) = com (fmap (eval' var com) x)

eval :: (Functor f) => (f b -> b) -> (Free f b -> b)
eval ___ (Var x) = x
eval com (Com x) = com (fmap (eval com) x)



trace :: (Functor f) => (b -> a) -> (b -> f b) -> (b -> Cofree f a)
trace get next b = Root (get b) (fmap (trace get next) (next b))

delta :: (Functor f) => Cofree f a -> Cofree f (Cofree f a)
delta t@(Root _ us) = Root t (fmap delta us)

instance (Functor f) => Functor (Cofree f) where fmap f (Root a ts) = Root (f a) (fmap (fmap f) ts)

instance (Functor f) => Functor (  Free f) where fmap f (Var x) = Var (f x)
                                                 fmap f (Com x) = Com (fmap (fmap f) x)
instance (Functor f) => Monad   (  Free f) where return         = Var
                                                 m >>= k        = join (fmap k m)

join :: (Functor f) => Free f (Free f a) -> Free f a

join (Var x) = x
join (Com x) = Com (fmap join x)


embed :: (Functor f) => f a -> Free f a
embed = Com . fmap Var

up :: (Functor f) => (f a -> a) -> (Free f a -> a)
down :: (Functor f) => (Free f a -> a) -> (f a -> a)

up   a = eval a
down a = a . embed
