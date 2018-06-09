{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module PostgresAccess where

import           Data.Text                                (Text)
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.PostgreSQL.Simple
import           Lens.Micro

data UserT f = User
  { _userEmail       :: Columnar f Text
  , _userFirstName   :: Columnar f Text
  , _userLastName    :: Columnar f Text
  , _userIsMember    :: Columnar f Bool
  , _userDaysInQueue :: Columnar f Int
  } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database be ShoppingCartDb

ShoppingCartDb (TableLens shoppingCartUsers) = dbLenses
User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userIsMember)
     (LensFor userDaysInQueue) = tableLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

allUsers :: Q PgSelectSyntax ShoppingCartDb s (UserT (QExpr PgExpressionSyntax s))
allUsers = all_ (shoppingCartDb ^. shoppingCartUsers)

insertUsers :: Connection -> IO [User]
insertUsers conn =
  runBeamPostgresDebug putStrLn conn $ runInsertReturningList (shoppingCartDb ^. shoppingCartUsers) $
      insertValues users

users =  [ User "james@example.com" "James" "Smith" True 1 {- james -}
                 , User "betty@example.com" "Betty" "Jones" False 42 {- betty -}
                 , User "james@pallo.com" "James" "Pallo" True 1 {- james -}
                 , User "betty@sims.com" "Betty" "Sims" False 42 {- betty -}
                 , User "james@oreily.com" "James" "O'Reily" True 1 {- james -}
                 , User "sam@sophitz.com" "Sam" "Sophitz" False 42 {- sam -}
                 , User "sam@jely.com" "Sam" "Jely" True 1 {- sam -}
                 , User "sam@example.com" "Sam" "Taylor" False 42 {- sam -}
                 ]


selectAllUsers :: Connection -> IO [User]
selectAllUsers conn =
  runBeamPostgresDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    (liftIO . return) users
