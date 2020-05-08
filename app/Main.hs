module Main where

import Lib
import Clean.CleanVotes
import Count.AV
import Count.STV

main :: IO ()
main = do 
    putStrLn "Please enter the voting file name:"
    file <- getLine
    putStrLn "Please enter the number of seats:"
    s <- getLine

    -- I/O Variables
    csvData <- readFile file 
    let seats =  read s :: Int

    -- Cleaning
    let dirtyVotes  = firstClean csvData
    let clean       = secondClean dirtyVotes
    let candidates  = getAllCandidates csvData
    let getSTVQuota = fromIntegral (length clean) / fromIntegral (seats + 1) + 1

    -- Alt Vote Winner
    let altVoteQuota  = getAltVoteQuota clean
    let altVoteWinner = getAltVoteWinner clean (voteCounter clean) candidates altVoteQuota
    print("The Alternative Vote winner was: ", altVoteWinner)

    -- STV Winners
    let stvWinner = getSTVVoteWinner clean seats 1 1 [] candidates []
    print("The Single Transferable Vote Winners were: ", stvWinner)