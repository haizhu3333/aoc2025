module Geometry (
    V2(..), V3(..), dot, norm2, distance2, getComponent
) where

import Data.Foldable (toList)

data V2 a = V2 !a !a deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative V2 where
    pure x = V2 x x
    V2 f1 f2 <*> V2 x1 x2 = V2 (f1 x1) (f2 x2)

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

data V3 a = V3 !a !a !a deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative V3 where
    pure x = V3 x x x
    V3 f1 f2 f3 <*> V3 x1 x2 x3 = V3 (f1 x1) (f2 x2) (f3 x3)

instance Num a => Num (V3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

dot :: (Applicative v, Foldable v, Num a) => v a -> v a -> a
dot u v = sum $ liftA2 (*) u v

norm2 :: (Applicative v, Foldable v, Num a) => v a -> a
norm2 v = dot v v

distance2 ::  (Applicative v, Foldable v, Num a) => v a -> v a -> a
distance2 u v = norm2 (liftA2 (-) u v)

getComponent :: Foldable v => Int -> v a -> a
getComponent i v = toList v !! i
