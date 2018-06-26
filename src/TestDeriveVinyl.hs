{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module TestDeriveVinyl where

import           Data.Coerce
import           Data.Vinyl
import qualified Data.Vinyl.Functor         as VF
import qualified Database.Beam              as B
import           Database.PostgreSQL.Simple
import           Frames
import           Generics.SOP
import           Generics.SOP.TH
import           PostgresAccess             (User, UserT, selectAllUsers)
import           Vinylize                   (createRecId, deriveVinyl)

deriveGeneric ''UserT

deriveVinyl ''UserT

test :: IO ()
test = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart1"
  us <- selectAllUsers conn
  mapM_ print $ toFrame $ map createRecId us


