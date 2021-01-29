module Loop where

frameCount :: Int
frameCount = 0

loop :: Int -> Int
loop frame
  | frame < 1000000 = loop $ frame + 1
  | otherwise = frame
