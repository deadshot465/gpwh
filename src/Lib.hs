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

counter :: Num a => a -> a
counter x = (\x -> x + 1)
            ((\x -> x + 1)
            ((\x -> x) x))

betterCounter :: Num a => a -> a
betterCounter x = (+ 1) ((+ 1) x)

compareLastNames :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareLastNames name1 name2 = if result == EQ
                               then compare (fst name1) (fst name2)
                               else result
    where result = compare (snd name1) (snd name2)

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = let lastName = snd name
                    nameText = fst name ++ lastName
                in
                    if lastName < "L" then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"

dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name = nameText ++ " PO Box 1337 - Washington DC, 20001"
    where nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "dc" -> dcOffice
    _ -> (\name -> fst name ++ " " ++ snd name)