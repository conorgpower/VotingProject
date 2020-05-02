module Clean.CleanVotes where

import Debug.Trace
import Data.List(sortBy, groupBy, delete)
import Data.Function (on)
import Data.List.Split(splitOn)

type Candidate  = (Char, String)
type Vote       = [(Char, String)]
type Count     = (Char, Int)

----------------------------------------------------------
---------------- General Cleaning Methods ----------------
----------------------------------------------------------

-- candidates = 

-- Takes the first entry of dirtyVotes, removes empty strings and creates a number for each viable candidate
getAllCandidates :: String -> [Candidate]
-- getAllCandidates xs = zip ['A'..] (drop 2 (head xs))

getAllCandidates xs = zip ['A'..] (head (drop 1 (filterVotes (splitVotes xs))))

-- Lists all valid votes (removes any with empty strings)
getVotes :: [[String]] -> [[String]]
getVotes xs =  drop 2 [drop 2 x | x <- xs, "" `notElem` x]

-- Assigns a character from the list ['A'..] to each element in each individual vote indicating which candidate each prefrence is for
pairVotes :: [[String]] -> [Vote]
pairVotes xs = [zip ['A'..] x | x <- getVotes xs]

-- Filters out any preference that has an asterisk instead of a valid vote
findStars :: Vote -> Vote
findStars = filter ((/="*") . snd)

-- Function to go through the entire list of Votes and remove asterisks
removeStars :: [Vote] -> [Vote]
removeStars = map findStars

-- Sorts the votes into ascending order (Sort by preference)
sortVotes :: [Vote] -> [Vote]
sortVotes = map (sortBy (compare `on` snd))

-- Checks each Vote to fund their first preference
checker :: Char -> Vote -> Int
checker x list = sum $ map (const 1) $ filter (== (x, "1")) list

-- Loops through the entire set of Votes to find the first preference of each voter
checkAllVotes :: Char -> [Vote] -> [Int]
checkAllVotes x = map (checker x)

splitVotes :: String -> [[String]]
splitVotes xs = map (splitOn ",") (splitOn "\n" xs)

filterVotes :: [[String]] -> [[String]]
filterVotes xs = filter (/= []) (map (filter(not . null)) xs)

-- Pairs, removes asterisks and sorts the list of DirtyVotes
cleanVotes :: String -> [Vote]
cleanVotes dirtyVotes =  sortVotes (removeStars (pairVotes (filterVotes (splitVotes dirtyVotes))))