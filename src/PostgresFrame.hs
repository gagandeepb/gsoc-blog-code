{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module PostgresFrame where

import           Data.Text          (Text, unpack)
import           Data.Vinyl
import qualified Data.Vinyl.Functor as VF
import           Frames.Col
import           Frames.InCore
import           Frames.Rec
import qualified Lens.Micro         as LM
import           PostgresAccess     (User, userDaysInQueue, userEmail,
                                     userFirstName, userIsMember, userLastName)

-- A
type SEmail = "email" :-> String
type SFirstName = "first_name" :-> String
type SLastName = "last_name" :-> String
type SIsMember = "is_member" :-> Bool
type SDaysInQueue = "days_in_queue" :-> Int

-- B
class FromBeam t rs | t -> rs where
  createRecId :: t -> Rec VF.Identity rs

-- C
instance FromBeam User '[SEmail, SFirstName, SLastName, SIsMember, SDaysInQueue] where
  createRecId u =
      VF.Identity (Col $ unpack $ u LM.^. userEmail) :&
      VF.Identity (Col $ unpack $  u LM.^. userFirstName) :&
      VF.Identity (Col $ unpack $ u LM.^. userLastName) :&
      VF.Identity (Col $ u LM.^. userIsMember) :&
      VF.Identity (Col $ u LM.^. userDaysInQueue) :&
      RNil
