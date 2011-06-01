{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Base.Types
  (ConnectInfo(..)
  ,Connection(..)
  ,Field(..)
  ,Result(..)
  ,Type(..)
  ,MessageType(..))
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

-- | Connection configuration.
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
data Result =
  Result {
    resultDesc :: Maybe RowDescription
   ,resultError :: Maybe L.ByteString
   ,resultNotices :: [String]
   ,resultType :: MessageType
  } deriving Show

-- | An internal message type.
data MessageType =
    CommandComplete
  | RowDescription
  | DataRow
  | EmptyQueryResponse
  | ErrorResponse
  | ReadyForQuery
  | NoticeResponse
  | AuthenticationOk
  | Query
  | UnknownMessageType
    deriving (Show,Eq)

-- | Description of a postgres row.
type RowDescription = [(L.ByteString
                       ,Int32
                       ,Int16
                       ,Int32
                       ,Int16
                       ,Int32)]

-- | FIXME: Come up with something for this based on postgres's features.
data Field = Field {
    fieldType :: Type
   ,fieldCharSet :: Word -- FIXME: Get the right value for this.
  }

-- FIXME: Update to proper supported types.
-- | Column types supported by PostgreSQL.
data Type = Decimal
          | Tiny
          | Short
          | Long
          | Float
          | Double
          | Null
          | Timestamp
          | LongLong
          | Int24
          | Date
          | Time
          | DateTime
          | Year
          | NewDate
          | VarChar
          | Bit
          | NewDecimal
          | Enum
          | Set
          | TinyBlob
          | MediumBlob
          | LongBlob
          | Blob
          | VarString
          | String
          | Geometry
            deriving (Enum, Eq, Show, Typeable)
