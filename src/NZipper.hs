module NZipper where

import Control.Comonad

-- Base Zipper

data Zipper x = Zipper [x] x [x] deriving(Show)

instance Comonad Zipper where
    extract (Zipper ls x rs) = x
    duplicate z = shift shiftLeft shiftRight z

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

makeLine :: Zipper Bool
makeLine = Zipper (repeat False) True (repeat False)

iterate' :: (a -> a) -> a -> [a]
iterate' f a = tail $ iterate f a

shiftRight (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
shiftRight _ = error "bad right"

shiftLeft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
shiftLeft _ = error "bad left"

shift :: (z a -> z a)
      -> (z a -> z a)
      -> z a
      -> Zipper (z a)
shift l r z =
    Zipper (iterate' l z) z (iterate' r z)

toList :: Zipper a -> [a]
toList (Zipper ls f rs) = reverse (take 10 ls) ++ [f] ++ (take 10 rs)


-- N-Dimentional Zippers

data NZipper a = Z a | S (NZipper (Zipper a)) deriving Show

instance Functor NZipper where
    fmap f (Z v) = Z (f v)
    fmap f (S v) = S (fmap (fmap f) v)

instance Comonad NZipper where
    extract (Z a) = a
    extract (S (Z a)) = extract a
    -- extract (S a) = ???

-- So this is easy.
-- I think because there's no constrain on the type of a being the same for each call
listMake :: NZipper a -> Int
listMake (Z _) = 0
listMake (S x) = 1 + listMake x

makeList :: Int -> a -> NZipper a
makeList 0 v = S (Z $ Zipper [v,v,v] v [v,v,v])
makeList n v = S (makeList (n-1) $ Zipper (replicate 3 v) v (replicate 3 v))

testShift :: NZipper a -> NZipper a
testShift (Z v) = Z v
testShift (S v) = S (fmap shiftLeft (testShift v))

shift1 :: NZipper a -> NZipper a
shift1 = id

