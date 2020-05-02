module Main where

import Lib
import Clean.CleanVotes
import Count.AV
-- import Count.STV

main :: IO ()
main = do 
    csvData <- readFile "votes.csv"
    -- putStrLn "Please enter the file you wish to load (no need for quotation marks)"
    -- file <- getLine
    -- putStrLn "Please enter the number of seats"
    -- s <- getLine

    -- csvData <- readFile file 
    -- let seats =  read s :: Int
    
    let clean        = cleanVotes    csvData
    let candidates   = getAllCandidates csvData
    let altVoteQuota = (length clean `div` 2) + 1

    print (getAltVoteWinner clean (voteCounter clean) candidates altVoteQuota)
