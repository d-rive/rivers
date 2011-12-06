{-# LANGUAGE UndecidableInstances #-}


module Data.Rivers.Algebras where


-- Final Co-Algebra
data Nu f = OutI { out :: f (Nu f) }
data Mu f = Innn { inI :: f (Mu f) }

instance Show (f (Nu f)) => Show (Nu f) where     show (OutI x) = show x
instance Show (f (Mu f)) => Show (Mu f) where     show (Innn x) = show x

unfold :: (Functor f) => (c -> f c) -> (c -> Nu f)
fold   :: (Functor f) => (f a -> a) -> (Mu f -> a)

unfold    c = OutI . fmap (unfold c) . c
fold      a =    a . fmap (fold   a) . inI



