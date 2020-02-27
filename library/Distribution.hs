module Distribution where

import Numeric.Natural (Natural)
import qualified System.Environment
import qualified Text.Read

-- | For a given number, enumerate the numbers from 1 to that number.
-- Example: 'possibleValuesStartingAtOne 5 == [1,2,3,4,5]'
-- Property: 'length (possibleValuesStartingAtOne n) == n'
possibleValuesStartingAtOne :: Natural -> [Natural]
possibleValuesStartingAtOne count = [1..count]

-- | For a given number, enumerate the numbers from 0 to that number.
-- Example: 'possibleValuesStartingAtOneIncludingZero 5 == [0,1,2,3,4,5]'
-- Property: 'length (possibleValuesStartingAtOneIncludingZero n) == n + 1'
possibleValuesStartingAtOneIncludingZero :: Natural -> [Natural]
possibleValuesStartingAtOneIncludingZero count = 0 : possibleValuesStartingAtOne count

possibleArrays :: [Natural] -> Natural -> [[Natural]]
possibleArrays possibleValues numberOfElements | numberOfElements == 0 = [[]]
possibleArrays possibleValues numberOfElements =
  let subArrays :: [[Natural]]
      subArrays = possibleArrays possibleValues (numberOfElements - 1)

      perElement :: Natural -> [[Natural]]
      perElement x = fmap (\xs -> x : xs) subArrays

  in concatMap perElement possibleValues

-- | Return all possible distributions with 'sizeOfElement' possible values per element and 'numberOfElements' total values.
-- Example: 'distCheck 2 5' is all of the possible counts of 0s and 1s for 5 bits.
--  0 1  ()
-- [0,5] means 0 zero-bits and 5 one-bits
-- [1,4] means 1 zero-bits and 5 one-bits
-- [2,3]
-- [3,2]
-- [4,1]
-- [5,0]
distributions :: Natural -> Natural -> [[Natural]]
distributions sizeOfElement numberOfElements =
  let -- For a given range of possible values [1..n] (here, 'numberOfElements == n')
      -- the count can be [0..n] since a possible distribution may have zero occurrences of a particular value
      possibleCountValues :: [Natural]
      possibleCountValues = possibleValuesStartingAtOneIncludingZero numberOfElements

      -- the distribution is an array where the elements are the count of how many times that value occurs,
      -- and the value is the 1-based index into the array
      all :: [[Natural]]
      all = possibleArrays possibleCountValues sizeOfElement

      -- A valid distribution is where the sum of the counts is equal to the numberOfElements input.
      check :: [Natural] -> Bool
      check counts = sum counts == numberOfElements
  in filter check all

-- | An integer sequence of number of possible distributions of the given element cardinality.
-- Examples:

-- distributionSizeSequence 3
-- [1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,253,276,300,325,351,378,406,435,465,496,528,561,595,630,666,703,741,780,820,861,903,946,990,1035,1081,1128,1176,1225,1275,1326,1378
-- A000217  Triangular numbers: a(n) = binomial(n+1,2) = n(n+1)/2 = 0 + 1 + 2 + ... + n.
-- A161680  a(n) = binomial(n,2): number of size-2 subsets of {0,1,...,n} that contain no consecutive integers.
-- A105340  a(n) = n*(n+1)/2 mod 2048.

-- distributionSizeSequence 4
-- [1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140,1330,1540,1771,2024,2300,2600,2925,3276,3654,4060,4495,4960,5456
-- A000292  Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6.

-- distributionSizeSequence 5
-- [1,5,15,35,70,126,210,330,495,715,1001,1365,1820,2380,3060,3876,4845,5985,7315,8855
-- A000332  Binomial coefficient binomial(n,4) = n*(n-1)*(n-2)*(n-3)/24.

-- distributionSizeSequence 6
-- [1,6,21,56,126,252,462,792,1287,2002,3003,4368,6188,8568,11628,15504
-- A000389  Binomial coefficients C(n,5).

-- distributionSizeSequence 7
-- [1,7,28,84,210,462,924,1716,3003,5005,8008
-- A000579  Figurate numbers or binomial coefficients C(n,6).

distributionSizeSequence :: Natural -> [Natural]
distributionSizeSequence sizeOfElement =
  let perLength :: Natural -> Natural
      perLength len = fromIntegral $ length $ distributions sizeOfElement len
  in fmap perLength [0..]

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
  putStrLn $ "sizeOfElement: " <> show sizeOfElement
  putStrLn $ "numberOfElements: " <> show numberOfElements
  mapM_ print $ distributions sizeOfElement numberOfElements
