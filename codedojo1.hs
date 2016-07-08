--Annie Cherkaev and Marielle Foster July 2016 @ Recurse Center

--https://www.hackerrank.com/challenges/connected-cell-in-a-grid

import Data.List (maximumBy, nub)
import Data.Function (on)
----executable
main = do
    raw <- readFile "board.txt"
    
    putStrLn $ show $ solveBoard $ readInBoard raw

readInBoard :: String -> [(Int, (Int, Int))]
readInBoard b = zip board coords
    where values = map (\x -> read x :: Int) (words b)
          m = head values
          n = head $ tail values
          board = tail $ tail values
          coords = [(x, y) | x <- [0..n-1], y <- [0..m-1]]


solveBoard :: [(Int, (Int, Int))] -> Int
solveBoard board = length $ maximumBy (compare `on` length) (map f board) -- walk over the board
    -- for each element
    where f = (\ (element, pos) -> if element == 1 then (findRegion [pos] [] board) else [])

-- recursive method to find the entire region
-- to do list has only ones
-- already seen list has NO shared elements with to do list
        --      to do              accum               board     
findRegion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, (Int, Int))] -> [(Int, Int)]
findRegion [] accumList board = accumList -- go until we've checked all the neighbors
findRegion toDo accumList board
    = findRegion newToDo newAccum board  -- add to accum, remove toDo
    where element = head toDo 
          newAccum = element : accumList
          neighborList = generateOneNeighbors element board (newAccum ++ toDo) -- only neighbors who are 1's
          justDid = tail toDo      
          newToDo = foldl (\acc neighbor -> (neighbor : acc)) justDid neighborList

--find all neighbors & filter
    --if the neighbor is not on accum list
        -- and it's value on the board is 1
        -- add to the list
                    --  position        --board                --accumlist    --return of neighbors
generateOneNeighbors :: (Int, Int) -> [(Int, (Int, Int))] -> [(Int, Int)]-> [(Int, Int)]
generateOneNeighbors (i, j) board accumList =  foldl (\acc (element, pos) -> if ((elem pos possNeighbors) && (not $ elem pos accumList) && (element == 1)) then pos:acc else acc) [] board
        where possNeighbors = [(i-1, j-1), (i-1, j), (i-1, j+1), (i, j-1), (i, j+1), (i+1, j-1), (i+1, j), (i+1, j+1)]



    

