module Lib
    ( someFunc
    ) where

import Control.Comonad
import Control.Monad

data Zipper x = Zipper [x] x [x] deriving(Show)
data U x = U (Zipper (Zipper x)) deriving(Show)

someFunc :: IO ()
someFunc = mconcat $ map (display 10) (take 20 $ iterate (extend rule) makeGrid)

-- rule :: Zipper Bool -> Bool
-- rule (Zipper (l:ls) x (r:rs)) = r==l

rule :: U Bool -> Bool
rule u = extract u
    -- where z = (Zipper (l:ls) x (r:rs))

display :: Int -> U Bool -> IO ()
display n (U z) = extract $ fmap (displayZ n) z
-- display n u = extract $ fmap (displayZ n) u

displayZ :: Int -> Zipper Bool -> IO ()
displayZ n (Zipper l f r) = putStrLn .
    fmap vis $
    (reverse (take n l)) ++ [f] ++ (take n r)
    where vis True = '*'
          vis False = ' '

makeLine :: Zipper Bool
makeLine = Zipper (repeat False) True (repeat False)

makeGrid :: U Bool
makeGrid = U $ Zipper (repeat z) z (repeat z)
    where z = Zipper (repeat False) True (repeat False)

makeGlider :: U Bool
makeGlider =
    U $ Zipper (repeat fz) fz rs
    where
        rs = [ line [f, t, f]
             , line [f, f, t]
             , line [t, t, t]
             ] ++ repeat fz
        t = True
        f = False
        fl = repeat f
        fz = Zipper fl f fl
        line l = Zipper fl f (l ++ fl)

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

iterate' :: (a -> a) -> a -> [a]
iterate' f a = tail $ iterate f a

shiftRight (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
shiftRight _ = error "bad right"

shiftLeft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
shiftLeft _ = error "bad left"

instance Comonad Zipper where
    extract (Zipper ls x rs) = x
    duplicate z = Zipper (iterate' shiftLeft z) z (iterate' shiftRight z)

instance Functor U where
    fmap f (U z) = U $ (fmap . fmap) f z

instance Comonad U where
    extract (U z) = extract $ extract z
    duplicate (U z) = fmap U . U . duplicate $ duplicate z
