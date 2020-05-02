module Count.STV where

import Debug.Trace
import Data.List(sortBy, groupBy, delete)
import Data.Function (on)
import Data.List.Split(splitOn)

type Candidate  = (Char, String)
type Vote       = [(Char, String)]
type Count     = (Char, Int)

-----------------------------------------------------------------
------------------- Single Transferable Vote --------------------
-----------------------------------------------------------------

-- getSTVQuota :: Int
-- getSTVQuota = (length cleanVotes `div` (seats + 1)) + 1
