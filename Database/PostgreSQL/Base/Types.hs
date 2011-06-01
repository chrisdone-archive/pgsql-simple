{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Base.Types
  (ConnectInfo
  ,Connection
  ,Field
  ,Result)
  where

import Control.Concurrent.MVar (MVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.String (IsString)
import Data.Text
import Data.Int
import Data.Time
import Data.Typeable
import Data.Word
import Network (PortID)
import System.IO (Handle)

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Eq,Read,Show,Typeable)

-- | A database connection.
data Connection = Connection {
      connectionHandle :: MVar (Maybe Handle)
    }

-- | Result of a database query.
data Result a =
  Result {
    resultRows :: [a]
   ,resultDesc :: Maybe RowDescription
   ,resultError :: Maybe L.ByteString
   ,resultNotices :: [String]
   ,resultType :: MessageType
  } deriving Show

data MessageType =
    CommandComplete
  | RowDescription
  | DataRow
  | EmptyQueryResponse
  | ErrorResponse
  | ReadyForQuery
  | NoticeResponse
  | UnknownMessageType
  | AuthenticationOk
  | Query
    deriving (Show,Eq)

type RowDescription = [(L.ByteString
                       ,Int32
                       ,Int16
                       ,Int32
                       ,Int16
                       ,Int32)]

data Field = Field