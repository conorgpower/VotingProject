module Count.STV where

import Clean.CleanVotes

import Data.List(sortBy)
import Data.Function (on)

------------------------------
-- SINGLE TRANSFERABLE VOTE --
------------------------------

-- Input:  List of Votes & Seats 
-- Output: STV Quota
getSTVQuota :: [Vote] -> Int -> Float
getSTVQuota xs x = fromIntegral (length xs) / fromIntegral (x + 1) + 1

-- Input:  List of Vote Counts & Seats
-- Output: Boolean representing if a candidate hit the quota
checkElected :: [Float] -> Float -> Bool
checkElected xs x
    | True `elem` [x >= x | x <- xs] = True
    | otherwise = False

-- Input:  Vote Counts & Candidates & Elected Candidates
-- Output: Elected Candidates
electCandidate :: [Float] -> [Candidate] -> [Candidate] -> [Candidate]
electCandidate xs candidates electedCandidates = do
    let x = zip ['A'..] xs
    let (z:zs) = sortBy (flip compare `on` snd) x
    findElectedCandidate (fst z) candidates : electedCandidates

-- Input:  Candidate Char & Candidates
-- Output: Elected Candidate
findElectedCandidate :: Char -> [Candidate] -> Candidate
findElectedCandidate x (y:ys)
    | x == fst y = y
    | otherwise = findElectedCandidate x ys

-- Input:  Votes & Prefence & VoteWeight
-- Output: Char of Lowest Candidate
findLowestCandidate :: [Vote] -> String -> Float -> Char
findLowestCandidate xs pref voteWeight = do
    let x = sortBy (compare `on` snd) (filter((/=0) . snd) (zip ['A'..] (stvVoteCounter xs pref voteWeight)))
    fst (head x)

-- Input:  Votes & Preference & Vote Wieght
-- Output: List of Votes Count
stvVoteCounter :: [Vote] -> String -> Float -> [Float]
stvVoteCounter xs pref voteWeight = do
    let x = [sum (checkAllSTVVotes x pref xs) | x <- ['A' .. 'E']]
    map (* voteWeight) x

-- Input:  Candidate Char & Preference & Vote
-- Output: Votes next preference
stvChecker :: Char -> String -> Vote -> Float
stvChecker x pref list = sum $ map (const 1) $ filter (== (x, pref)) list

-- Input:  Candidate Char & Preference & Votes
-- Output: List of next preferences
checkAllSTVVotes :: Char -> String -> [Vote] -> [Float]
checkAllSTVVotes x pref = map (stvChecker x pref)

-- Input:  Elected Candidate Votes & Quota
-- Output: Surplus Votes
getSurplus :: Float -> Float -> Float
getSurplus x y = x - y

-- Input:  Old Weight & Surplus & Transferable Votes
-- Output: New Vote Weight
getVoteWeight :: Float -> Float -> Float -> Float
getVoteWeight x y z = x * (y / z)

-- Input:  Candidate Char & Vote
-- Output: Bool representing trasnferability
getTransferableVotesChecker :: Char -> Vote -> Bool
getTransferableVotesChecker x xs = (fst (head xs) == x) && not (null (tail xs))

-- Input:  Candidate Char & Votes
-- Output: Total Transferable Votes
getTransferableVotes :: Char -> [Vote] -> Float
getTransferableVotes x xs = do
    let zs = map (getTransferableVotesChecker x) xs
    fromIntegral (sum $ map fromEnum zs)

-- Input:  Votes & Char of Lowest Candidate
-- Output: Votes without lowest Candidates Votes
removeCandidateVotes :: [Vote] -> Char -> [Vote]
removeCandidateVotes xs z = map (filter ((/=z) . fst)) xs

-- Input:  List of Vote Counts & Votes
-- Output: Votes without Last Candidate
removeLastCandidate :: [Float] -> [Vote] -> [Vote]
removeLastCandidate xs ys = do
    let z = sortBy (compare `on` snd) (zip ['A'..] xs)
    removeCandidateVotes ys (fst (head z))

-- Input:  Vote potentially with Eliminated Candidate present
-- Output: Vote without Eliminated Candidate
changeEliminatedCandidateVotes :: Vote -> Vote
changeEliminatedCandidateVotes [] = []
changeEliminatedCandidateVotes [x] = [(fst x, "1")]
changeEliminatedCandidateVotes (x:xs) 
    | snd x /= "1" = (fst x, "1") : xs
    | otherwise = x:xs

-- Input:  Votes potentially with Eliminated Candidate present
-- Output: Votes without Eliminated Candidate
changeAllCandidateVotes :: [Vote] -> [Vote]
changeAllCandidateVotes = map changeEliminatedCandidateVotes

-- Input:  Votes & Seats & Vote Weight & Preference & Weighted Vote Count & Candidates & Elected Candidates
-- Output: List of Elected 
getSTVVoteWinner :: [Vote] -> Int -> Float -> Int -> [Float] -> [Candidate] -> [Candidate] -> [(Int, String)]
getSTVVoteWinner votes seats voteWeight pref newWeightedVotes allCandidates electedCandidates =
    if length electedCandidates == seats
        then
            zip [1..] (reverse (map snd electedCandidates))
        else do
            let quota = getSTVQuota votes seats
            let totalVotes = if not (null newWeightedVotes)
                then do
                    let x2 = stvVoteCounter votes (show pref) voteWeight
                    let x3 = zipWith (+) x2 newWeightedVotes
                    x3
                else
                    stvVoteCounter votes (show pref) voteWeight
            if checkElected totalVotes quota
                then do
                    let x = electCandidate totalVotes allCandidates electedCandidates 
                    let z = getTransferableVotes (fst (head x)) votes
                    let y = zip ['A'..] totalVotes
                    let surplus = getSurplus (maximum totalVotes) quota
                    let newVoteWeight = getVoteWeight voteWeight surplus z
                    let votes1 = filter (/=[]) (removeCandidateVotes votes (fst (head x)))
                    let newVotes = resetVote votes1
                    let newWeightTransferrableVotes = stvVoteCounter (resetVote votes1) (show pref) newVoteWeight
                    getSTVVoteWinner newVotes seats newVoteWeight 1 newWeightTransferrableVotes allCandidates x
                else do
                    let x1 = findLowestCandidate votes (show pref) voteWeight
                    let votes3 = changeAllCandidateVotes votes
                    let pref1 = 1
                    getSTVVoteWinner votes3 seats voteWeight pref1 [] allCandidates electedCandidates