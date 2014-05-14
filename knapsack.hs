import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = do
    (s, n) <- getItem
    items <- sequence $ replicate n getItem
    putStrLn $ show $ solve s n items

getItem :: IO Item
getItem = fmap (parseItem . (map read)) $ fmap words getLine

type Size = Int
type Value = Int
type Item = (Size, Value)

parseItem :: [Int] -> Item
parseItem (a:b:_) = (a, b)

solve :: Int -> Int -> [Item] -> Int
solve s n items = dp ! s
    where
        dp :: UArray Int Int
        dp = runSTUArray (do
            arr <- newListArray (-1,s) (replicate (s+2) 0)
            forM_ items (\item -> do
                let thisSize = fst item
                let thisValue = snd item
                forM_ [s,(s-1)..0] (\size -> do
                    lastOne <- readArray arr (size-1)
                    thisOne <- readArray arr size
                    let curBest = max lastOne thisOne
                    writeArray arr size curBest
                    when (size >= thisSize) (do
                        prev <- readArray arr (size - thisSize)
                        let candy = prev + thisValue
                        when (candy > curBest) (writeArray arr size candy)
                        )
                    ) 
                )
            return arr)
