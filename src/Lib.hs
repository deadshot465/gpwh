module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World"

inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n * n

lesson2Func :: Integral a => a -> a
lesson2Func n = if even n then n - 2
                else 3 * n + 1

counter x = (\x -> x + 1)
            ((\x -> x + 1)
            ((\x -> x) x))

betterCounter x = (+ 1) ((+ 1) x)