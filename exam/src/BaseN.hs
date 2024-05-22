module BaseN where

import Data.List (reverse, foldl')

data BaseN = BaseN {
    base :: Int,
    value :: Int
} deriving (Eq)

intToDigits :: Int -> Int -> [Int]
intToDigits b v = reverse $ go b v where
    go b v
        | v < b     = [v]
        | otherwise = (v `mod` b) : go b (v `div` b)

digitsToInt :: Int -> [Int] -> Int
digitsToInt b = foldl' (\acc x -> acc * b + x) 0

instance Show BaseN where
    show (BaseN b v) =  concatMap show (intToDigits b v) ++ "_(" ++ show b ++ ")"

instance Num BaseN where
    (+) (BaseN b v1) (BaseN _ v2) = BaseN b (v1 + v2)
    (-) (BaseN b v1) (BaseN _ v2) = BaseN b (v1 - v2)
    (*) (BaseN b v1) (BaseN _ v2) = BaseN b (v1 * v2)
    abs (BaseN b v) = BaseN b (abs v)
    signum (BaseN b v) = BaseN b (signum v)
    fromInteger i = BaseN 10 (fromIntegral i)

    negate (BaseN b v) = BaseN b (negate v)

instance Ord BaseN where
    compare (BaseN _ v1) (BaseN _ v2) = compare v1 v2


