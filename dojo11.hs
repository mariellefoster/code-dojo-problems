import Data.List
import Data.Ord

collatzNext :: Integer -> Integer
collatzNext n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1


collatzRecurse :: Integer -> Integer
collatzRecurse n
 | n == 1 = 1
 | otherwise = 1 + (collatzRecurse $ collatzNext n)

collatzSequence :: Integer -> [Integer]
collatzSequence n = unfoldr (\x -> if x == 1 then Nothing else Just(x, collatzNext x)) n


main :: IO ()
main = do 
    putStrLn (show (maximumBy (comparing fst) (zip (map collatzRecurse [1..999999]) [0..])))
    --putStrLn (show (maximum (map length . collatzSequence $ [1..999999])))

