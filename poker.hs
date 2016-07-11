-- Marielle Foster + James Porter solution to poker hand problem
-- July 2016

import Data.List

--main = do

--isStraightFlush :: [Int] -> Bool

generateCounts :: Ord a => [a] -> [(a, Int)]
generateCounts nums = map (\ l -> (head l, length l)) (group (sort nums))


isNKind :: Int -> [Int] -> Bool
isNKind n nums = any (==n) $ map snd (generateCounts nums)



isFourKind :: [Int] -> Bool
isFourKind nums = isNKind 4 nums


isFullHouse :: [Int] -> Bool
isFullHouse nums = (isThreeKind nums) &&  (isOnePair nums)

----isFlush  :: [Int] -> Bool

isStraight  :: [Int] -> Bool
isStraight nums = sortedNums == [headNums..lastNums]
    where sortedNums = (sort nums)
          headNums = head sortedNums
          lastNums = last sortedNums
    


isThreeKind :: [Int] -> Bool
isThreeKind nums = isNKind 3 nums

 
isTwoPair  :: [Int] -> Bool
isTwoPair nums = any (==2) $ map snd (generateCounts $ map snd (generateCounts nums))


isOnePair :: [Int] -> Bool
isOnePair nums = isNKind 2 nums


