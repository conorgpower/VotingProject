module Clean.CleanVotes where

import Data.List(sortBy)
import Data.Function (on)
import Data.List.Split(splitOn)

type Candidate  = (Char, String)
type Vote       = [(Char, String)]
type Count     = (Char, Int)

----------------------
-- CLEANING LIBRARY --
----------------------

-- Input:  CSV as a String
-- Output: List of Votes as Strings
firstClean :: String -> [[String]]
firstClean xs = do
  let zs = map (splitOn ",") (splitOn "\r\n" xs)
  filter (/=[]) (map (filter(/="")) zs)

-- Input:  CSV as a String
-- Output: List of Candidates
getAllCandidates :: String -> [Candidate]
getAllCandidates xs = zip ['A'..] (head (firstClean xs))

-- Input:  List of Votes as Strings
-- Output: List of valid votes without emptys and additional data
getVotes :: [[String]] -> [[String]]
getVotes xs = drop 1 [drop 2 x | x <- xs, "" `notElem` x]

-- Input:  List of Votes as Strings
-- Output: List of Votes zipped [A..E]
pairVotes :: [[String]] -> [Vote]
pairVotes xs = [zip ['A'..] x | x <- getVotes xs]

-- Input:  List of Votes potentially with asterisks
-- Output: List of Votes without asterisks
removeStars :: [Vote] -> [Vote]
removeStars = map (filter ((/="*") . snd))

-- Input:  List of Votes
-- Output: List of Votes sorted by preference
sortVotes :: [Vote] -> [Vote]
sortVotes = map (sortBy (compare `on` snd))

-- Input:  Char of Candidate & Vote
-- Output: Int to represent the preference
checker :: Char -> Vote -> Int
checker x xs = sum $ map (const 1) $ filter (== (x, "1")) xs

-- Input:  Candidate Char & List of Votes
-- Output: List of Ints representing the vote prteferences
checkAllVotes :: Char -> [Vote] -> [Int]
checkAllVotes x = map (checker x)

-- Input:  List of Votes as Strings
-- Output: List of Votes fully cleaned
secondClean :: [[String]] -> [Vote]
secondClean xs = sortVotes (removeStars (pairVotes xs))

-- Input:  Vote potentally without next preference at "1"
-- Output: Vote with next prefence as "1"
changePref :: Vote -> Vote
changePref [] = []
changePref [x] = [(fst x, "1")]
changePref (x:xs) = (fst x, "1") : xs

-- Input:  List of Votes potentally without next preference at "1"
-- Output: List of Votes with next prefence as "1"
resetVote :: [Vote] -> [Vote]
resetVote = map changePref