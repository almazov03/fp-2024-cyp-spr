module Rotate where

rotate :: Int -> [a] -> [a]
rotate n xs
  | null xs = xs
  | otherwise = take len $ drop (mod n' len) $ cycle xs
  where
    len = length xs
    n' = if n < 0 then len + n else n
