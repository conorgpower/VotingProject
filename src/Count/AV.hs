module Count.AV where

import Clean.CleanVotes

import Data.List(sortBy)
import Data.Function (on)

----------------------
-- ALTERNATIVE VOTE --
----------------------

-- Input:  Votes
-- Output: Quota
getAltVoteQuota :: [Vote] -> Float
getAltVoteQuota xs = fromIntegral (length xs `div` 2) + 1

-- Input:  Votes & Vote Count & Candidates & Quota
-- Output: Candidate Name as String
getAltVoteWinner :: [Vote] -> [Int] -> [Candidate] -> Float -> String
getAltVoteWinner xs ys candidates quota = do
    let winner = checkWinner ys quota
    if winner
        then getWinner ys candidates
        else do
            let a | 0 `elem` ys = zip ['A'..] (removeItem 0 ys)
                  | otherwise = zip ['A'..] ys
            let (b:bs) = sortBy (compare `on` snd) a
            getAltVoteWinner (resetVote (removeLowest (fst b) xs candidates)) (voteCounter (resetVote (removeLowest (fst b) xs candidates))) candidates quota

-- Input:  Number to be removed & Vote Count
-- Output: Vote Count withh removed number
removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Input:  Votes
-- Output: Vote Count
voteCounter :: [Vote] -> [Int]
voteCounter xs = [sum (checkAllVotes x xs) | x <- ['A' .. 'E']]

-- Input:  Candidate Char & Candidates
-- Output: Candidate Name as String
findCandidate :: Char -> [Candidate] -> String
findCandidate x (y:ys)
    | x == fst y = snd y
    | otherwise = findCandidate x ys

-- Input:  Vote Count & Candidates
-- Output: Candidate Name as String
getWinner :: [Int] -> [Candidate] -> String
getWinner xs candidates = do
    let x = zip ['A'..] xs
    let (z:zs) = sortBy (flip compare `on` snd) x
    findCandidate (fst z) candidates

-- Input:  Vote Count & Quota
-- Output: Bool representing if a candidate reached quota
-- Checks if the candidate with the highest number of votes has reached the quota yet
checkWinner :: [Int] -> Float -> Bool
checkWinner xs x
    | True `elem` [x >= x | x <- xs] = True
    | otherwise = False

-- Input:  Candidate Char & Vote
-- Output: Vote without Candidate
removeVote :: Char -> Vote -> Vote
removeVote _ [] = []
removeVote x (y:ys) | x == fst y = removeVote x ys
                    | otherwise = y : removeVote x ys

-- Input:  Candidate Char & Votes & Candidates
-- Output: Votes withtout candidate
removeLowest :: Char -> [Vote] -> [Candidate] -> [Vote]
removeLowest x xs candidates = do
    removeCandidate x candidates
    zip ['A'..] candidates
    map (removeVote x) xs

-- Input:  Candidate Char & Candiates
-- Output: Candidates without lowest Candidate
removeCandidate :: Char -> [Candidate] -> [Candidate]
removeCandidate _ [] = []
removeCandidate x xs = [c | c <- xs, fst c /= x]