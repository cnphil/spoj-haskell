import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import Data.Maybe
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = do
  let maxN = 500000
  let ldps = ldpArray maxN
  (n:ts) <- B.getContents >>= return . map (fst . fromJust . B.readInt) . B.lines
  putStr $ unlines $ map show $ map (properDivSumWith ldps) $ take n ts

properDivSumWith :: UArray Int Int -> Int -> Int 
properDivSumWith ldps n = divSumWith ldps n - n

divSumWith :: UArray Int Int -> Int -> Int 
divSumWith ldps n =
  product [ foldl (\z p-> 1+p*z) 1 ps | ps <- group $ factorWith ldps n ]

factorWith :: UArray Int Int -> Int -> [Int]
factorWith ldps 1 = []
factorWith ldps n = ldp : factorWith ldps (div n ldp)
  where ldp = ldpWith ldps n

ldpArray :: Int -> UArray Int Int
ldpArray n = runSTUArray (do
  arr <- newListArray (0,n) [0..n]
  forM_ [4,6..n] (\i -> writeArray arr i 2)
  forM_ [3,5..n] (\p -> do
    ldp <- readArray arr p
    when (ldp == p) (do
      forM_ [3*p,5*p..n] (\i -> do
        v <- readArray arr i
        when (v == i) (writeArray arr i p)
        )))
  return arr)

ldpWith :: UArray Int Int -> Int -> Int
ldpWith a n = a ! n
