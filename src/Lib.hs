module Lib
    ( someFunc
    ) where

import Control.Comonad
import Control.Monad

data CellData = CB Bool | CI Int deriving(Show)

data Zipper x = Zipper [x] x [x] deriving(Show)
data U x = U (Zipper (Zipper x)) deriving(Show)

-- n-dimentional zippers

data NZipper a = Z a | S (NZipper (Zipper a)) deriving Show
-- data NZipper a = Z a | S (NZipper [a]) deriving Show

makeList :: Int -> a -> NZipper a
makeList 0 v = S (Z $ Zipper [v,v,v] v [v,v,v])
makeList n v = S (makeList (n-1) $ Zipper (replicate 3 v) v (replicate 3 v))

instance Functor NZipper where
    fmap f (Z v) = Z (f v)
    fmap f (S v) = S (fmap (fmap f) v)
    -- fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

-- testShift :: NZipper a -> NZipper a
-- testShift (Z v) =
-- testShift (S v) = S $ testShift

-- end experiment

someFunc :: IO ()
someFunc =  mapM_ putStrLn $ map (\x -> display $ result x golRule makeGlider) [1..5]
-- someFunc =  putStrLn $ display $ result 3 golRule makeGlider

display ::  U CellData -> String
display (U z) = unlines $ map toString $ toList $ fmap toList z

toString :: [CellData] -> String
toString cd = mconcat $ map displayCell cd

displayCell :: CellData -> String
displayCell (CI n) = show n
displayCell (CB b) = case b of
                       True -> "*"
                       False -> "-"

result :: Int -> (U a -> a) -> U a -> U a
result steps rule u = last . take steps $ iterate (extend rule) u

toList :: Zipper a -> [a]
toList (Zipper ls f rs) = reverse (take 10 ls) ++ [f] ++ (take 10 rs)

rule :: U a -> a
rule u = extract u

makeLine :: Zipper Bool
makeLine = Zipper (repeat False) True (repeat False)

makeIntGrid :: U CellData
makeIntGrid = U $ Zipper (repeat (shiftLeft z)) z (repeat z)
    where z = Zipper left (CI 0) right
          left = (map CI $ iterate (subtract 1) (-1))
          right = (map CI [1 ..])

makeGrid :: U Bool
makeGrid = U $ Zipper (repeat z) z (repeat z)
    where z = Zipper (repeat False) True (repeat False)

makeGlider :: U CellData
makeGlider =
    U $ Zipper (repeat fz) fz rs
    where
        rs = [ line [f, t, f]
             , line [f, f, t]
             , line [t, t, t]
             ] ++ repeat fz
        t = CB True
        f = CB False
        fl = repeat f
        fz = Zipper fl f fl
        line l = Zipper fl f (l ++ fl)

iterate' :: (a -> a) -> a -> [a]
iterate' f a = tail $ iterate f a

shiftRight (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
shiftRight _ = error "bad right"

shiftLeft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
shiftLeft _ = error "bad left"

up :: U a -> U a
up (U z) = U (shiftLeft z)

down :: U a -> U a
down (U z) = U (shiftRight z)

left :: U a -> U a
left (U z) = U (fmap shiftLeft z)

right :: U a -> U a
right (U z) = U (fmap shiftRight z)

horizontal :: U a -> Zipper (U a)
horizontal = shift (U . (fmap shiftLeft getZipper)) (U . (fmap shiftRight getZipper))
-- horizontal = shift left right

vertical :: U a ->  Zipper (U a)
vertical = shift up down

neighbours :: U a -> [a]
neighbours u = map (\shift -> extract $ shift u) allShifts
    where allShifts = horz ++ vert ++ diag
          horz = [left, right]
          vert = [up, down]
          diag = (.) <$> horz <*> vert

golRule :: U CellData -> CellData
golRule (U (Zipper _ (Zipper _ (CI n) _) _)) = error "Might as well write Python!"
golRule u = case nCount of
               2 -> extract u
               3 -> CB True
               _ -> CB False
    where nCount = length . filter id $ getBool <$> neighbours u
          getBool (CB b) = b

instance Comonad Zipper where
    extract (Zipper ls x rs) = x
    duplicate z = shift shiftLeft shiftRight z
    -- duplicate z = Zipper (iterate' shiftLeft z) z (iterate' shiftRight z)

instance Functor Zipper where
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

shift :: (z a -> z a)
      -> (z a -> z a)
      -> z a
      -> Zipper (z a)
shift l r z =
    Zipper (iterate' l z) z (iterate' r z)

instance Functor U where
    fmap f (U z) = U $ (fmap . fmap) f z

getZipper (U z) = z

instance Comonad U where
    extract (U z) = extract $ extract z
    duplicate u@(U z) = U $
                        fmap (shift (U . (fmap shiftLeft getZipper) ) right) $
                        (shift (U . shiftLeft . getZipper) (U . shiftRight . getZipper)) u
    -- duplicate u = U $ fmap horizontal $ vertical u
    -- duplicate (U z) = fmap U . U . duplicate $ duplicate z
