{-# OPTIONS_GHC -Wincomplete-patterns #-}

module OrAndVector where

import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Vector as V

data Type = Or (V.Vector Type) | And (V.Vector Type) deriving Show

calculateSize :: Type -> Integer
calculateSize (Or items) = sum $ fmap calculateSize items
calculateSize (And items) = product $ fmap calculateSize items

enumerate :: Type -> V.Vector Integer
enumerate (Or items) =
  F.foldl'
    (\results next -> results <> fmap (+ fromIntegral (V.length results)) next)
    V.empty
    (fmap enumerate items)

enumerate (And items) =
  F.foldl'
    (\results next -> F.foldMap (\n -> fmap (\r -> n + r * (fromIntegral (V.length next))) results) next)
    (V.singleton 0)
    (fmap enumerate items)

-- enumerate (Or []) = []
-- enumerate (And []) = [0]
-- enumerate (Or (x : xs)) = current ++ fmap (+ currentSize) rest
--   where
--   rest = enumerate (Or xs)
--   currentSize = fromIntegral $ length current
--   current = enumerate x
-- enumerate (And (x : xs)) =
--   concatMap (\c -> fmap (\r -> r + c * restSize) rest) current
--   where
--   restSize = fromIntegral $ length rest
--   rest = enumerate (And xs)
--   current = enumerate x
