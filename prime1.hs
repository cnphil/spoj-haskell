{-# OPTIONS_GHC -O2 #-}
import Control.Monad
import Data.List (concat, intersperse)

intercalate xs xss = concat (intersperse xs xss)

makeRange s = intPair (break (==' ') s)
              where intPair (a, b) = ((read a) :: Int, (read b) :: Int)

main = interact ((++"\n") . intercalate "\n\n" . map (intercalate "\n") . map (map show) . map primesBetween . map makeRange . tail . lines)

primesBetween (low,high) = filter isPrime [low..high]
    where
        isPrime 1 = False
        isPrime n = isPrime' n smallPrimes
            where
              isPrime' _ []       = True
              isPrime' n (p:ps)
                | p * p > n       = True
                | n `mod` p == 0  = False
                | otherwise       = isPrime' n ps
smallPrimes = (2:(primes [3,5..31623]))

primes :: [Int] -> [Int] 
primes [] = []
primes (p:ps)
       | p > 1 = p:(primes $ filter ((> 0) . (`mod` p)) ps)
