module Storage
  ( SIndexed,
    IIndexed,
    Storage,
    storage,
    maxStorage,
    totalUsage,
    addToStorage,
    removeFromStorage,
    storedResourceAmount,
  )
where

import Data.Bifunctor

type SIndexed a = (String, a)

type IIndexed a = (Int, a)

type Storage = [SIndexed Int]

maxStorage :: Int
maxStorage = 1000

storage :: Storage
storage = [("Wood", 0), ("Stone", 900)]

-- The code below works the same as the following:
-- totalUsage [] = 0
-- totalUsage (pair : list) = snd pair + totalUsage list
totalUsage :: Storage -> Int
totalUsage = foldr ((+) . snd) 0

storedResourceAmount :: String -> Storage -> Int
storedResourceAmount _ [] = 0
storedResourceAmount resource (pair : list)
  | listKey == resource = snd pair
  | otherwise = storedResourceAmount resource list
  where
    listKey = fst pair

-- Testing ways to add resources to storage
addToStorage :: SIndexed Int -> Int -> Storage -> (Bool, Storage)
addToStorage addingPair usage []
  | canAddMore = (True, [addingPair])
  | otherwise = (False, [])
  where
    amount = snd addingPair
    canAddMore = maxStorage >= usage + amount
addToStorage addingPair usage (pair : list)
  | listKey == key && canAddMore = (True, (listKey, listAmount + amount) : list)
  | otherwise = second (pair :) $ addToStorage addingPair usage list
  where
    key = fst addingPair
    amount = snd addingPair
    listKey = fst pair
    listAmount = snd pair
    canAddMore = maxStorage >= usage + amount

removeFromStorage :: SIndexed Int -> Storage -> (Bool, Storage)
removeFromStorage _ [] = (False, [])
removeFromStorage removingPair (pair : list)
  | listKey == key && storageHasEnough = (True, (listKey, listAmount - amount) : list)
  | otherwise = second (pair :) $ removeFromStorage removingPair list
  where
    key = fst removingPair
    amount = snd removingPair
    listKey = fst pair
    listAmount = snd pair
    storageHasEnough = listAmount >= amount

-- The code below works the same as the following:
-- makeNewPair pair tuple = (fst tuple, (pair : snd tuple))
-- makeNewPair :: SIndexed Int -> (Bool, Storage) -> (Bool, Storage)
-- makeNewPair pair = second (pair :)
