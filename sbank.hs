import qualified Data.Sequence as Seq
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (toList, foldl')

main :: IO ()
main = do
    cases <- readLn :: IO Int
    replicateM_ cases testcase

type BankRecord = (BS.ByteString, Int)

testcase :: IO ()
testcase = do
    n <- readLn :: IO Int
    inputs <- sequence (take n (repeat BS.getLine))
    getLine
    let sorted = Seq.sort $ Seq.fromList inputs
    sequence_ $ fmap putBank $ toList $ foldl' handleRep (Seq.empty :: Seq.Seq BankRecord) sorted
    putStrLn ""

putBank bkr = do
    BS.putStr $ fst bkr
    putStrLn $ show $ snd bkr

handleRep s str = case (Seq.viewr s) of
                        Seq.EmptyR  ->  Seq.singleton (str, 1)
                        prev Seq.:> lastR -> if (fst lastR) == str
                                                then prev Seq.|> (str, (snd lastR) + 1)
                                                else s Seq.|> (str, 1)
                    
