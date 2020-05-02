module Count.AV where

import Clean.CleanVotes

import Debug.Trace
import Data.List(sortBy, groupBy, delete)
import Data.Function (on)
import Data.List.Split(splitOn)

---------------------------------------------------------
------------------- Alternative Vote --------------------
---------------------------------------------------------

-- -- Finds the quota for a successfull candidate
-- getAlternativeQuota :: Int
-- getAlternativeQuota cleanVotes = (length cleanVotes `div` 2) + 1

getAltVoteWinner :: [Vote] -> [Int] -> [Candidate] -> Int -> String
getAltVoteWinner x y candidates quota = do
    let winner = checkWinner y quota
    if winner
        then getWinner y candidates
        else do
            let a | 0 `elem` y = zip ['A'..] (removeItem 0 y)
                  | otherwise = zip ['A'..] y
            let (b:bs) = sortBy (compare `on` snd) a
            getAltVoteWinner (resetVote (removeLowest (fst b) x candidates)) (voteCounter (resetVote (removeLowest (fst b) x candidates))) candidates quota

-- Returns a list of integers indicating the amount of first preference votes each candidate recieved
voteCounter :: [Vote] -> [Int]
voteCounter xs = [sum (checkAllVotes x xs) | x <- ['A' .. 'E']]

-- Finds a candidate based on the Char passed into the function
findCandidate :: Char -> [Candidate] -> String
findCandidate x (y:ys)
    | x == fst y = snd y
    | otherwise = findCandidate x ys

-- Returns the Full name of the Winner of the Vote
getWinner :: [Int] -> [Candidate] -> String
getWinner xs candidates = do
    let x = zip ['A'..] xs
    let (z:zs) = sortBy (flip compare `on` snd) x
    findCandidate (fst z) candidates

-- Checks if the candidate with the highest number of votes has reached the quota yet
checkWinner :: [Int] -> Int -> Bool
checkWinner xs quota
    | True `elem` [x >= quota | x <- xs] = True
    | otherwise = False

-- Removes the candidat with the lowest number of votes from the race. Also removes their votes
removeLowest :: Char -> [Vote] -> [Candidate] -> [Vote]
removeLowest x xs candidates = do
    removeCandidate x candidates
    zip ['A'..] candidates
    map (removeVote x) xs

-- Removes individual votes from the list of votes
removeVote :: Char -> Vote -> Vote
removeVote _ [] = []
removeVote x (y:ys) | x == fst y = removeVote x ys
                    | otherwise = y : removeVote x ys

-- Calls the "changePref" function on each individual vote
resetVote :: [Vote] -> [Vote]
resetVote = map changePref

-- Changes the preference of the next choice to the top choice
changePref :: Vote -> Vote
changePref [] = []
changePref [x] = [(fst x, "1")]
changePref (x:xs) = (fst x, "1") : xs

-- Removes an item from the list (Normally removing 0 from list after votes have been re-counted)
removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Removes a candidate from the list if they have recieved the lowest number of votes in that round of counting
removeCandidate :: Char -> [Candidate] -> [Candidate]
removeCandidate _ [] = []
removeCandidate x xs = [c | c <- xs, fst c /= x]
