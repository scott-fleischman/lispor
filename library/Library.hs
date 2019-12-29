{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Library where

data Type = Or [Type] | And [Type] deriving Show

calculateSize :: Type -> Integer
calculateSize (Or items) = sum $ fmap calculateSize items
calculateSize (And items) = product $ fmap calculateSize items

enumerate :: Type -> [Integer]
enumerate (Or []) = []
enumerate (And []) = [0]
enumerate (Or (x : xs)) = current ++ fmap (+ currentSize) rest
  where
  rest = enumerate (Or xs)
  currentSize = fromIntegral $ length current
  current = enumerate x
enumerate (And (x : xs)) =
  concatMap (\c -> fmap (\r -> r + c * restSize) rest) current
  where
  restSize = fromIntegral $ length rest
  rest = enumerate (And xs)
  current = enumerate x

main = putStrLn "Hi"
