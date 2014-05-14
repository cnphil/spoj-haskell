-- SPOJ PALIN
-- ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Char

main :: IO ()
main = do 
    cases <- readLn :: IO Int
    sequence_ (fmap (>>= BS.putStrLn) (take cases (repeat (fmap nextPalin BS.getLine))))

nextPalin :: BS.ByteString -> BS.ByteString
nextPalin str'
    | str' == (BS.pack "9") = BS.pack "11"
    | otherwise             = if (BS.reverse h1) > h2 then makeReverse h1
                                        else makeReverse (inc h1)
    where
        str = let dropStr = BS.dropWhile (=='0') str'
              in if BS.null dropStr then BS.pack "0" else dropStr
        (h1, h2) = if even (BS.length str)
                      then (BS.take half str, BS.drop half str)
                      else (BS.take (half+1) str, BS.drop half str)
        half = (BS.length str) `div` 2
        makeReverse s = BS.append s (BS.drop (BS.length s - half) (BS.reverse s))

inc :: BS.ByteString -> BS.ByteString
inc = makeNew . BS.spanEnd (== '9')
      where makeNew (high, low) = let high' = if BS.null high
                                                 then BS.pack "1"
                                                 else BS.snoc (BS.init high)
                                                      (chr (1 + ord (BS.last high)))
                                  in BS.append high' (BS.replicate (BS.length low) '0')
