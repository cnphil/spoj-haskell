{-# OPTIONS_GHC -O2 #-}
-- SPOJ DIVSUM
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

primes :: [Int]
primes = sieve $ 2 : 3 : [k + i | k <- [6, 12..], i <- [-1, 1]]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : (sieve . filter ((/=0).(`mod`x)) $ xs)

divsum n 
    | n <= 1 = 0
    | otherwise = (-n) + g n 1 1 primes
    where g n sum csum ps@(p:ps') 
            | p * p > n && n /= p    = sum * csum * (1 + n)
            | p * p > n && n == p    = sum * (csum * p + 1)
            | rm == 0      = g dv sum (csum * p + 1) ps
            | otherwise    = g n (sum * csum) 1 ps'
            where (dv, rm) = quotRem n p

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . BS.readInt

main :: IO ()
main = BS.getContents
        >>= (putStr . unlines . map (show . divsum . toInt) . tail . BS.lines)

