module Distribution where

import Numeric.Natural (Natural)
import qualified System.Environment
import qualified Text.Read

possibleArrays :: Natural -> Natural -> [[Natural]]
possibleArrays sizeOfElement numberOfElements | numberOfElements == 0 = [[]]
possibleArrays sizeOfElement numberOfElements =
  let possibleElements :: [Natural]
      possibleElements = [0..sizeOfElement]

      subArrays :: [[Natural]]
      subArrays = possibleArrays sizeOfElement (numberOfElements - 1)

      perElement :: Natural -> [[Natural]]
      perElement x = fmap (\xs -> x : xs) subArrays

  in concatMap perElement possibleElements

distCheck :: Natural -> Natural -> [[Natural]]
distCheck sizeOfElement numberOfElements =
  let all = possibleArrays numberOfElements sizeOfElement
      check xs = sum xs == numberOfElements
  in filter check all

main :: IO ()
main = do
  args <- System.Environment.getArgs
  (sizeOfElement, numberOfElements) <-
    case args of
      x : y : _
        | Just sizeOfElement <- Text.Read.readMaybe x
        , Just numberOfElements <- Text.Read.readMaybe y
        -> return (sizeOfElement, numberOfElements)
      _ -> fail "Pass two natural numbers as arguments"
  mapM_ print $ distCheck sizeOfElement numberOfElements
