-- not original
import Data.List
import System.Random
import Control.Monad
 
helpMod::Integer->Integer->Integer->Integer
helpMod a d n= fun a d where
    fun a 1 = mod a n
    fun a 2 = mod (a^2) n
    fun a d = let
            p=fun (mod (a^2) n) (div d 2)
            q=if odd d then mod (a*p) n else p
          in mod q n
 
isProbable::Integer-> Bool
isProbable n |n<=1 = False
         |n==2 = True
         |even n = False
         |otherwise = rabinMiller [2,3,5,7,11,13,17,19] n s d where
             d=until (\x->mod x 2/=0) (`div` 2) (n-1) --(n-1=2^s*d)
             s=until (\x->d*2^x==pred n) (+1) 0  --counts the power
              
 
rabinMiller::[Integer]->Integer->Integer->Integer-> Bool
rabinMiller [] _ _ _ = True
rabinMiller (a:xs) n s d =  if a == n then True 
                 else  case x==1 of
                    True -> rabinMiller xs n s d
                    _ -> if any ( ==pred n) $ take (fromIntegral s) $ iterate (\e->mod (e^2) n)  x then rabinMiller xs n s d
                          else  False
                   where x=helpMod a d n
                     
 
 
 
fn :: Integer -> Integer -> Integer -> Integer
fn x c n = mod ( x^2 + c ) n 
 
pollard :: Integer -> IO  Integer  
pollard n 
  | even n = return  2 
  | otherwise = 
      randomRIO ( 2 , n - 1 ) >>=
     (\x' -> randomRIO ( 2 , n - 1 ) >>= 
        (\c ->  return ( until ( \( _ , _ , d ) -> d /= 1 || d == n )  ( \( x , y , d ) -> ( fn x c n , fn ( fn y c n ) c n , gcd ( fn x c n - fn ( fn y c n ) c n  ) n ) )  ( x' , x' , 1 ) ) >>= 
        ( \( _ , _ , k ) -> return k  ) ) )
 
 
 
factor :: Integer -> IO [ Integer ] 
factor 1 = return [] 
factor n = 
   (return . isProbable $ n ) >>= 
    (\x -> case x of 
        True -> return [n]
        _    -> pollard n >>= 
              (\d -> case d of 
                   1 -> factor n 
                   _ -> factor d  >>= (\l -> ( factor.div n $ d ) >>= ( \m -> return $ l ++ m ) ) ) )
             
     
divSum :: [[Integer]] -> Integer
divSum [] = 1
divSum (x:xs) = div (  ( head x )^( genericLength x + 1 ) -1 ) ( head x - 1 ) * divSum xs 
 
 
main = do
     n <- liftM read getLine 
     helpfun n 0

helpfun n cnt 
        | cnt == n = return [] 
        | otherwise = do
                m <- liftM read getLine
                lst <- factor m 
                putStrLn.show $ ( divSum.groupBy (\x y -> x == y ).sort $ lst ) - m 
                helpfun n (cnt+1)
 
