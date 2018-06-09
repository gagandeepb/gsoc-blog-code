{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
module DataFrame where
import           Data.Vinyl (rcast)
import           Frames

-- Let's assume we have a CSV ('users.csv') containing the fields
-- 'email', 'first_name', 'last_name', 'is_member', 'days_in_queue'.
-- Let's further assume the CSV also contains column headers
-- and does not have NAs (i.e. no missing data).

-- The following will read a certain number of rows
-- and generate the types and type synonyms corresponding
-- to each column. For example it will generate:
-- @type IsMember = "is_member" :-> Bool@
-- and similarly for the other columns.
tableTypes "UserF" "data/users.csv"

loadUsers :: IO (Frame UserF)
loadUsers = inCoreAoS (readTable "data/users.csv")

columnSubset :: UserF -> Record '[Email, LastName]
columnSubset = rcast

-- badColumnSubset :: UserF -> Record '[Email, Age]
-- badColumnSubset = rcast
