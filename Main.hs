module Main where

import Data.String

type SIndexed a = (String, a)

type IIndexed a = (Int, a)

main :: IO ()
main = do
  putStrLn "Who are you?"
  line <- getLine
  putStrLn (greeting "Hello " line)

testAa :: [Char] -> [Int]
testAa = map fromEnum

greeting :: String -> [Char] -> [Char]
greeting a b = a ++ fromString b ++ "!"

maxStorage = 1000

storage :: [SIndexed Int]
storage = [("Wood", 0), ("Stone", 100)]

totalUsage :: [SIndexed Int] -> Int
totalUsage = foldr ((+) . snd) 0
-- ^ Its the same as:
-- totalUsage [] = 0
-- totalUsage (kvPair : list) = snd kvPair + totalUsage list

-- Testing
addToStorage :: SIndexed Int -> [SIndexed Int] -> Maybe [SIndexed Int]
addToStorage _ [] = Nothing
addToStorage addingKV (kvPair : list)
  | fst kvPair == fst addingKV = Just ((fst kvPair, snd kvPair + snd addingKV) : list)
  | otherwise = addToStorage addingKV list