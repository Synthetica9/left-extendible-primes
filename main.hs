module Main where

import Data.Tree
import Control.Monad
import Math.NumberTheory.Primes.Testing
import Control.Arrow
import System.Environment

unfoldTree_ :: (a -> [a]) -> a -> Tree a
unfoldTree_ f = unfoldTree (\x -> (x, f x))

baseNForest :: Integer -> [Integer]
baseNForest base = concat $ levels $ snd <$> unfoldTree_ f (0, 0)  where
  f (depth, n) = do
    x <- [1..base - 1]
    let candidate = x * (base ^ depth) + n
    guard (isPrime candidate)
    pure (depth + 1, candidate)

main :: IO ()
main = do
  [n] <- getArgs
  let base = read n :: Integer
  let answer = length $! (baseNForest base)
  seq answer (pure ())
  print (base, answer)
