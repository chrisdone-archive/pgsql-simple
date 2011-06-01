{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Base where

import Data.Word
import Data.Typeable

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Eq,Read,Show,Typeable)
