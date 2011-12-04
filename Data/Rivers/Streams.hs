{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- |
-- My module for streams
module Data.Rivers.Streams where

import Prelude hiding (zip,unzip)
import Data.Rivers.Idiom
import Data.Rivers.NumExt
import Math.OEIS (SequenceData,lookupSequence, getSequenceByID)
import Control.Monad (liftM2)

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary)
import Test.LazySmallCheck (Serial, series, cons2)


-- * Streams
--
-- | 
--   Your standard /Streams/, renamed to @S@ because @S@ looks like a meandering
--        stream.
--
data S v = Cons v (S v)
          deriving (Eq, Ord, Show, Read)







infixr 5 <<|
infixr 5 <||
infixr 5 |~|
infixr 5 |!|




-- ** Cons 1, n:
(<||)          :: a -> S a -> S a
(<<|)          :: [a] -> S a -> S a    -- ^ prepend, Hinze UFP p.3
(|~|)          :: S a -> S a -> S a

--(|%|)          :: (Fractional a) => S a -> S a -> S a
--(|*|)          :: (Num a) => S a -> S a -> S a
ago            :: Integer -> S a ->  a


any_a          :: S a



z0             :: (Num a) => S a
asum           :: (Num a) => S a -> S a
bsum           :: (Num a) => S a -> S a
csum           :: (Num a) => S a -> S a
diff           :: (Num a) => S a -> S a
inv            :: (Num a) => S a -> S a
sconst         :: (Num a) => a   -> S a
times          :: (Num a) => a   -> S a -> S a
plus           :: (Num a) => S a -> S a -> S a

interleave     :: S a -> S a -> S a
interleave'    :: S a -> S a -> S a
alternate      :: S a -> S a -> S a

combStreams :: [[a]] -> [[a]]

drop0L         ::             S a -> S a
dropIp1L       ::             S a -> S a
dup            ::             S a -> S a

-- ** (Improperly) using @Ord@
(|!|)          :: (Ord a) =>  S a -> S a -> S a
merge          :: (Ord a) =>  S a -> S a -> S a
union          :: (Ord a) =>  S a -> S a -> S a

-- ** (Improperly) using @Eq@
allEqual       :: (Eq a) => [a] -> Bool
group          :: (Eq a) => S a -> S [a]


fix            ::  (a -> a)   ->   a
inits          :: S a -> S ([a])
interleave3    :: S a -> S a -> S a
intersperse    :: a -> S a -> S a
map1           :: (a -> b) -> S a -> S b

mapAdjacent    :: (a -> a -> b) -> [a] -> [b]

--power          :: (Fractional a, Integral b) => S a -> b -> S a
turn           :: (Integral a) => a -> [a]
--srecip         :: (Fractional a) => S a -> S a


-- ** @G@enerating Functions
-- | 
-- 
--   A "generating function" for Streams.
type G v o = [v] -> o


-- ** Generating Functions, etc
fromFG         :: G a a            -> S a
revFix         :: G a a            -> S a
rgen           :: G a b            -> (S a -> S b)
fwdFix         :: G a a            -> S a
grow           :: G a b            -> (S a -> S b)

hOfFG          :: G a b            -> b
tOfFG          :: G a b            -> a     -> G a b

rep            :: (S a -> S b)     -> G a b
rgen'          ::                     G a b            -> [a]    ->    (S a -> S b)
hOfRG          :: (G a b, [a])     -> b
tOfRG          :: (G a b, [a])     -> a     -> (G a b, [a])
fromRG         :: (G a a, [a])     -> S a

-- ** Gen to Tree:
toT            ::                  G a b ->    Tree a b
-- ** Tree to Gen:
toG            ::   Tree a b  ->   G a b

-- * Infinite @Tree@s
-- | 
--   An infinite Tree. Used to represent /Streams/
data Tree a o = Node o (a -> Tree a o)


-- ** Trees
branches       ::             Tree a b -> (a -> Tree a b)
fromT          ::             Tree a a       -> S a
label          ::             Tree a b -> b


-- * Coalgebras
-- | 
--   Your standard Co-Algebra (dual to Algebra).
type Coalg c a b = (c -> b, c -> a -> c)


-- ** Coalgebraic
unfold         :: Coalg c a b -> c -> Tree a b
cfix           :: Coalg c a a -> c -> S a
groW           :: Coalg c a b -> c -> (S a -> S b)

sMap           :: (a -> b)                   -> S a -> S b
sMap2          :: (a -> b -> c)              -> S a -> S b -> S c
sMap3          :: (a -> b -> c -> d)         -> S a -> S b -> S c -> S d
sMap4          :: (a -> b -> c -> d -> e)    -> S a -> S b -> S c -> S d -> S e

s_even         ::             S a -> S a
seven          ::             S a -> S a
s_odd          ::             S a -> S a
sodd           ::             S a -> S a


-- ** Using @Bool@ Predicates
sbreak         :: (a -> Bool) -> S a -> ([a], S a)
sdropWhile     :: (a -> Bool) -> S a -> S a
stakeWhile     :: (a -> Bool) -> S a -> [a]
sfilter        :: (a -> Bool) -> S a -> S a
spartition     :: (a -> Bool) -> S a -> (S a, S a)
sspan          :: (a -> Bool) -> S a -> ([a], S a)

scan           :: (a -> b -> a) -> a -> S b -> S a
scan'          :: (a -> b -> a) -> a -> S b -> S a
scan1          :: (a -> a -> a)      -> S a -> S a
scan1'         :: (a -> a -> a)      -> S a -> S a
scycle         :: [a] -> S a

-- ** Drivers
siterate       :: (a -> a) -> (a -> S a)

-- ** Heads and Tails
shead          :: S a ->   a
stail          :: S a -> S a
tail2          :: S a -> S a
tails          :: S a -> S (S a)

-- ** Indexed
stake          ::        Integer -> S a -> [a]
sdrop          :: Int -> S a -> S a
ssplitAt       :: Int -> S a -> ([a], S a)

smerge         ::             S a -> S a -> S a

-- ** Zips and Unzips
sunzip         :: S (a, b) -> (S a, S b)
szipWith       :: (a -> b -> c) -> S a -> S b -> S c



transpose      :: S (S a) -> S (S a)



-- ** Utility Functions
fromJust       :: Maybe a -> a
fromOEIS       :: String -> [Integer]






instance Functor S where
 fmap f ~(Cons h t) = (f h) <|| (fmap f t)

instance Monad S where
  return = srepeat
  xs >>= f = join (fmap f xs)
    where
      join ~(Cons zs xss) = Cons (shead zs) (join (smap stail xss))

instance Arbitrary a => Arbitrary (S a) where
     arbitrary = liftM2 (<||) arbitrary arbitrary


instance CoArbitrary a => CoArbitrary (S a) where
  coarbitrary xs gen = do
    n <- arbitrary
    coarbitrary (stake (abs n) xs) gen


instance Serial a => Serial (S a) where
     series = cons2 Cons

instance Idiom S where
     pure a = s where s = a <|| s
     s <> t = (shead s) (shead t) <|| (stail s) <> (stail t)
     srepeat a = s where s = a <|| s
     smap f s = f (shead s) <|| smap f (stail s)
     zip g s t = g (shead s) (shead t) <|| zip g (stail s) (stail t)

instance (Num a) => Num (S a) where
     (+) = zip (+)
     (-) = zip (-)
     (*) = zip (*)
     negate         = sMap (negate)
     abs            = sMap (abs)
     signum         = sMap (signum)
     fromInteger    = srepeat . fromInteger


instance (Enum a) => Enum (S a) where
     toEnum i = srepeat (toEnum i)
     fromEnum = error "fromEnum: not defined for streams"

instance (Real a) => Real (S a) where
     toRational = error "toRational: not defined for streams"

instance (Integral a) => Integral (S a) where
     div = zip div
     mod = zip mod
     quotRem s t = sunzip (zip quotRem s t)
     toInteger = error "toInteger: currently not defined for streams"

instance (Fractional a) => Fractional (S a) where
     s / t = zip (Prelude./) s t
     recip s = smap (recip) s
     fromRational r = srepeat (fromRational r)

-- | unzip, specialized to Stream tuples

sunzip s = (a <|| as , b <|| bs)
     where
          (a        , b       ) = shead s
          (      as ,       bs) = sunzip (stail s)

-- | 'filter' @p@ @xs@, removes any elements from @xs@ that do not satisfy @p@.
--
-- /Beware/: this function may diverge if there is no element of
-- @xs@ that satisfies @p@, e.g.  @filter odd (repeat 0)@ will loop.
sfilter p ~(Cons x xs)
  | p x       = Cons x (sfilter p xs)
  | otherwise = sfilter p xs


-- | 'takeWhile' @p@ @xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
stakeWhile p (Cons x xs)
  | p x       = x : stakeWhile p xs
  | otherwise = []

-- | 'dropWhile' @p@ @xs@ returns the suffix remaining after
-- 'takeWhile' @p@ @xs@.
--
-- /Beware/: this function may diverge if every element of @xs@
-- satisfies @p@, e.g.  @dropWhile even (repeat 0)@ will loop.
sdropWhile p ~(Cons x xs)
  | p x       = sdropWhile p xs
  | otherwise = Cons x xs


-- | 'sspan' @p@ @xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
sspan p (Cons x xs)
  | p x       = let (trues, falses) = sspan p xs
                in (x : trues, falses)
  | otherwise = ([], Cons x xs)

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
sbreak p = sspan (not . p)

-- | The 'splitAt' function takes an integer @n@ and a stream @xs@
-- and returns a pair consisting of the prefix of @xs@ of length
-- @n@ and the remaining stream immediately following this prefix.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
ssplitAt n xs
  | n == 0    = ([],xs)
  | n > 0     = let (prefix,rest) = ssplitAt (n-1) (stail xs)
                in (shead xs : prefix, rest)
  | otherwise = error "S.splitAt negative argument."

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of streams. The first stream corresponds
-- to the elements of @xs@ for which @p@ holds; the second stream
-- corresponds to the elements of @xs@ for which @p@ does not hold.
--
-- /Beware/: One of the elements of the tuple may be undefined. For
-- example, @fst (partition even (repeat 0)) == repeat 0@; on the
-- other hand @snd (partition even (repeat 0))@ is undefined.
spartition p ~(Cons x xs) =
  let (trues,falses) = spartition p xs
  in if p x then (Cons x trues, falses)
            else (trues, Cons x falses)

-- | The 'group' function takes a stream and returns a stream of
-- lists such that flattening the resulting stream is equal to the
-- argument.  Moreover, each sublist in the resulting stream
-- contains only equal elements.  For example,
--
group ~(Cons x ys) = let (xs, zs) = sspan (\y -> x == y) ys
                    in (x : xs) <|| group zs






-- | 'drop' @n@ @xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
sdrop n xs
  | n == 0    = xs
  | n > 0     = sdrop (n - 1) (stail xs)
  | otherwise = error "Stream.drop: negative argument."


inits xs = Cons [] (fmap (shead xs :) (inits (stail xs)))

-- | The 'stails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@.
tails xs = Cons xs (tails (stail xs))


(<||)           h t                = Cons h t
shead     (Cons h _)               = h

stail     (Cons _ t)               = t

any_a                              = any_a

stake   0  ___________             =  []
stake  (n) (Cons x xs)             =  x : stake (n-1) xs

ago     0  (Cons x __)             =  x
ago    (n) (Cons _ xs)             =  (n-1) `ago` xs


-- | merge, version 2                                  [Hinze UFP p.35]

-- | map, version 1
-- | map, version 2
-- | map2, really zip?



s |~| t   = shead s <|| (t) |~| (stail s)
s |!| t = s `union` t

map1 f   s          = f (shead s)  <|| (map1  f (stail s))
sMap f  (Cons x xs) =  (f x)       <|| (sMap  f    xs    )

merge s@(Cons m s') t@(Cons n t') =
     if   m <= n
     then m         <|| merge s' t
     else      n    <|| merge s  t'

-- ^ from Unique Fixed Point p.35

-- | union for streams
union s@(Cons m s') t@(Cons n t') =
     case compare m n
        of
          LT -> m <|| union s' t
          EQ -> m <|| union s' t'
          GT -> n <|| union s  t'

--zip f s t                                                  = f (shead s) (shead t) <|| zip f (stail s) (stail t)
sMap2  f  (Cons x xs) (Cons y ys)                           =  (f x y)     <||  (sMap2 f xs ys      )
sMap3  f  (Cons x xs) (Cons y ys) (Cons z zs)               =  (f x y z)   <||  (sMap3 f xs ys zs   )
sMap4  f  (Cons t ts) (Cons x xs) (Cons y ys) (Cons z zs)   =  (f t x y z) <||  (sMap4 f ts xs ys zs)


smerge         (Cons m s) t             = m     <||  (smerge   t s )
plus           (Cons m s) (Cons n t)    = m + n <|| plus         s         t
alternate      (Cons m s) (Cons _ t)    = m     <|| alternate    t         s         
interleave     (Cons m s) (Cons n t)    = m     <|| interleave   (n <|| t) s
interleave'    (Cons m s) t             = m     <|| interleave'  t         s

szipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (szipWith f xs ys)

-- | Interleave two Streams @xs@ and @ys@, alternating elements
-- from each list.
--
-- > [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
interleave3 ~(Cons x xs) ys = Cons x (interleave3 ys xs)

-- | 'intersperse' @y@ @xs@ creates an alternating stream of
-- elements from @xs@ and @y@.
intersperse y ~(Cons x xs) = Cons x (Cons y (intersperse y xs))


-- | infix prepend

[    ] <<| s   = s
(a:as) <<| s   = a <|| (as <<| s)



-- | turn something
turn n | n == 0     = []
turn n | n > 0      = turn (n-1) ++ [n-1] ++ turn (n-1)
turn n | n < 0      = error "turn: negative argument"
turn _              = error "blah!"

-- | 'cycle' @xs@ returns the infinite repetition of @xs@:
--
-- > cycle [1,2,3] = Cons 1 (Cons 2 (Cons 3 (Cons 1 (Cons 2 ...
scycle xs = foldr Cons (scycle xs) xs


-- |      Arithmatic, Jumping, ...
--
--
--

-- | multiplication
-- | stream inversion
-- | finite (forward) difference
-- | duplicate the head of the stream
-- | even (indexed) elements
-- | odd (indexed) elements
-- | even (indexed) elements, v2
-- | odd (indexed) elements, v2
-- | drop function, results in (4*n - 1)
-- | drop function, results in (2*n)
-- | an alternative tail function

-- | a kind of sum function
-- | right inverse of diff

-- Finite (or forward) difference
diff  (Cons m s@(Cons n _)) = n - m <|| diff s    
-- ^ from Hinze UFP p.45
tail2 (Cons _   (Cons n s)) = n     <|| s         
-- ^ from Hinze UFP p.49
times n (Cons m s) = n * m <|| times n s


asum s                        = 0 <|| s + asum s                         
-- ^ from Hinze UFP p.4
bsum s                        = t where t = 0 <|| t + s
csum s                        =   0 <|| srepeat (shead s) + csum (stail s)


-- | iterate (inductively) over a stream
--
-- this can't be stopped? 
siterate f a = a <|| siterate f (f a)


-- from Hinze UFP p.4


-- as patterns are co-pointed functors:
-- data C x = As x (B x)

dup s@(Cons m _) =    m       <|| s    
-- ^ from Hinze UFP p.39
inv   (Cons m s) = (1 - m)    <|| inv s 
-- ^ from Hinze UFP p.41


-- | 2D operator?
--
seven (Cons m (Cons _ s)) = m <|| seven s
-- ^ from Hinze UFP p.45
sodd  (Cons _ (Cons n s)) = n <|| sodd s
-- ^ from Hinze UFP p.45

-- | mutually recursive
s_even s                  = shead s <|| s_odd (stail s) 
-- ^ from Hinze UFP p.45
s_odd  s                  =             s_even (stail s)
-- ^ from Hinze UFP p.45


-- from Hinze UFP p.45
--
--seven     <=>  drop1of2
--sodd      <=>  drop0of2


-- |
--
-- > scan f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scan f z ~(Cons x xs) =  z <|| scan f (f z x) xs

-- | @scan'@ is a strict scan.
--
scan' f z xs =  z <|| (scan' f $! (f z (shead xs))) (stail xs)

-- | 'scan1' is a variant of 'scan' that has no starting value argument:
--
-- > scan1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scan1 f ~(Cons x xs) = scan f x xs

-- | @scan1'@ is a strict scan that has no starting value.
scan1' f ~(Cons x xs) = scan' f x xs

-- | 'transpose' computes the transposition of a stream of streams.
transpose ~(Cons (Cons x xs) yss) =
    (x <|| smap shead yss) <|| transpose (xs <|| smap stail yss)


-- from Hinze UFP p.45
{--
shead (dropIIofL s) = shead s
stail (dropIIofL s) = dropIofL (stail s)

shead (drop0ofL s) = shead (stail s)
stail (drop0ofL s) = dropLppofL (stail (stail s))
-}

dropIp1L (Cons m s         )  = m <|| dropIp1L (stail s        ) 
-- ^ from Hinze UFP p.45
drop0L   (Cons _ (Cons n s))  = n <|| drop0L   (stail (stail s)) 
-- ^ from Hinze UFP p.45

-- | standard fix-point function
-- | standard fix-point function, specialized to Streams (forward ordering)
-- | standard fix-point function, specialized to Streams (reverse ordering)


-- | transform a generator to a Stream operator
-- | transform a generator to a Stream operator - v2?
-- | transform a Stream operator to a generator
-- | transform a generator, along with a reversed list, into a Stream operator



----------------------------------------------------------------------------------------------------
fix f       = let x = f x in x

fwdFix g    = fix (grow g)
revFix g    = fix (rgen g)


grow  g     ~(Cons x xs) = (g []) <|| (grow (g . (x:)) xs)
rgen' g ys  ~(Cons x xs) = (g ys) <|| (rgen' g (x:ys) xs)
rgen  g                  = rgen' g []

rep f []       = shead (f any_a)
rep f (x:xs)   = rep (stail . f . (x <||) ) xs


------------------------------------------------------------
--
--
-- from dons's




--onesv  =  0 <|| onesv + 1 - carry




-- 
--   Tree Representation
--

-- | smart constructor for Tree labels
-- | smart constructor for Tree branches
-- | translate a Tree to a Generator
-- | translate a Generator to a Tree
-- | translate a Tree element to a Stream element
-- | translate a Generator element to a Stream element
-- | fromFG helper function (head) 
-- | fromFG helper function (tail)
-- | fromRG: translate a Generator (and a reversed list) to a Stream element
-- | fromRG helper function (head)
-- | fromRG helper function (tail)

-- | unfold operator, specialized to Co-Algebras
-- | standard fix-point function, specialized to Co-Algebras
-- | generate a Stream operator, given a Co-Algebra

unfold    (h,t) z              = Node (h z) (\x -> unfold (h,t) (t z x)   )
cfix      (h,t) z              = fix        (groW         (h,t)    z      )
groW      (h,t) z ~(Cons x xs) = (h z) <||  (groW         (h,t) (t z x) xs)
--generate  (h,t) z              = gen (gen' (unfold        (h,t)    z  ))


label     (Node y _)          = y
branches  (Node _ f)          = f

toG       (Node y _) []       = y
toG       (Node _ f) (x:xs)   = toG (f x) xs

toT g    = Node (g []) (\x -> toT (g . (x:)))

fromT  = cfix (label , branches)
fromFG = cfix (hOfFG , tOfFG)
fromRG = cfix (hOfRG , tOfRG)

hOfFG g             = g []
tOfFG g x           = g . (x:)
hOfRG (g,xs)        = g xs
tOfRG (g, xs) x     = (g, x:xs)



-- | utility function to lookup sequence in OEIS
-- | utility function to check of all elements of a list are equal
-- | utility function to unwrap a (known good) Maybe
-- | utility function to map over adjacent elements in a list


combStreams xxs = foldr (zipWith (:)) (Prelude.repeat []) xxs

allEqual = and . mapAdjacent (==)
mapAdjacent f xs = zipWith f xs (tail xs)

fromOEIS str = fromJust $ getSequenceByID str

fromJust Nothing = error "My programmer promised he knew what he was doing! He's a liar!"
fromJust (Just x) = x


-- | Power Series "Glasses"
--

sconst n = n <|| srepeat 0

z0 = 0 <|| 1 <|| srepeat 0


-- | Horner's Rule on Streams
--
-- s = sconst (shead t) + (z |*| stail s)
--
-- implies
--
-- z |*| s = 0 <|| s
--

{-
infixl 7 |*|
infixl 7 |%|


(|*|) s t = shead s * shead t <|| srepeat (shead s) * (stail t) + (stail s) |*| t



srecip s = t 
     where 
      a = srecip (shead s)
      t =  a <|| srepeat (- a) * (stail s |*| t)

(|%|) s t = s |*| srecip t



power s n
   | n >= 0     =  pow s n
   | otherwise  =  srecip (pow s (- n))
   where pow _t 0        =  sconst 1
         pow t  (k)      =  t ** pow t (k - 1)
         pow _ _         =  error "power: impossible"



-- diverges
--
-}

main :: IO ()
main = putStrLn "mine.hs"
