{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Library where

data Type = Or [Type] | And [Type]

calculateSize :: Type -> Integer
calculateSize (Or items) = sum $ fmap calculateSize items
calculateSize (And items) = product $ fmap calculateSize items

-- enumerate :: Type -> [Integer]
-- enumerate (Or []) = []
-- enumerate (And []) = [0]

main = putStrLn "Hi"
