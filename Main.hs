module Main where

import Loop
import Storage

main :: IO ()
main = do
  print storage
  putStrLn "What resource do you want to add to storage?"
  addResource <- getLine
  putStrLn "How many?"
  addAmount <- getLine
  let addResult = addToStorage (addResource, read addAmount) (totalUsage storage) storage
  print addResult
  putStrLn "What resource to remove?"
  removeResource <- getLine
  putStrLn "How many?"
  removeAmount <- getLine
  let removeResult = removeFromStorage (removeResource, read removeAmount) (snd addResult)
  print removeResult
  putStrLn $ show $ storedResourceAmount "Wood" $ snd removeResult
  let frames = loop frameCount
  putStrLn $ show frames
