{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Base.Types
  (ConnectInfo(..)
  ,Connection(..)
  ,Field(..)
  ,Result(..)
  ,Type(..)
  ,MessageType(..)
  ,Size(..)
  ,FormatCode(..)
  ,Modifier(..)
  ,ObjectId(..)
  ,Pool(..)
  ,PoolState(..)
  ,ConnectionError(..))
  where

import Control.Concurrent.MVar (MVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Typeable
import Data.Word
import System.IO (Handle)
import Data.Map (Map)
import Control.Exception (Exception)

data ConnectionError =
    QueryError (Maybe String)   -- ^ Query returned an error.
  | QueryEmpty                  -- ^ The query was empty.
  | AuthenticationFailed String -- ^ Connecting failed due to authentication problem.
  | InitializationError String  -- ^ Initialization (e.g. getting data types) failed.
  | ConnectionLost              -- ^ Connection was lost when using withConnection.
  | UnsupportedAuthenticationMethod Int32 String -- ^ Unsupported method of authentication (e.g. md5).
  | GeneralError String
  deriving (Typeable,Show)

instance Exception ConnectionError where

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
      connectionHandle  :: MVar (Maybe Handle)
    , connectionObjects :: MVar (Map ObjectId String)
    }

-- | Result of a database query.
data Result =
  Result {
    resultRows :: [[Maybe B.ByteString]]
   ,resultDesc :: Maybe [Field]
   ,resultError :: Maybe L.ByteString
   ,resultNotices :: [String]
   ,resultType :: MessageType
   ,resultTagRows :: Maybe Integer
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
  | PasswordMessage
  | UnknownMessageType
    deriving (Show,Eq)

-- | A field description.
data Field = Field {
    fieldType :: Type
   ,fieldFormatCode :: FormatCode
  } deriving Show

data Type = 
    Short      -- ^ 2 bytes, small-range integer
  | Long       -- ^ 4 bytes, usual choice for integer
  | LongLong   -- ^ 8 bytes	large-range integer
  | Decimal -- ^ variable, user-specified precision, exact, no limit
  | Numeric -- ^ variable, user-specified precision, exact, no limit
  | Real             -- ^ 4 bytes, variable-precision, inexact
  | DoublePrecision -- ^ 8 bytes, variable-precision, inexact

  | CharVarying -- ^ character varying(n), varchar(n), variable-length
  | Characters  -- ^ character(n), char(n), fixed-length
  | Text        -- ^ text, variable unlimited length
              -- 
              -- Lazy. Decoded from UTF-8 into Haskell native encoding.

  | Boolean -- ^ boolean, 1 byte, state of true or false

  | Timestamp -- ^ timestamp /without/ time zone
              -- 
              -- More information about PostgreSQL’s dates here:
              -- <http://www.postgresql.org/docs/current/static/datatype-datetime.html>
  | TimestampWithZone -- ^ timestamp /with/ time zone
  | Date              -- ^ date, 4 bytes	julian day
  | Time              -- ^ 8 bytes, time of day (no date)

   deriving (Eq,Enum,Show)

-- | A field size.
data Size = Varying | Size Int16
  deriving (Eq,Ord,Show)

-- | A text format code. Will always be TextCode for DESCRIBE queries.
data FormatCode = TextCode | BinaryCode
  deriving (Eq,Ord,Show)

-- | A type-specific modifier.
data Modifier = Modifier

-- | A PostgreSQL object ID.
newtype ObjectId = ObjectId Int32
  deriving (Eq,Ord,Show)

-- | A connection pool.
data PoolState = PoolState {
    poolConnections :: [Connection]
  , poolConnectInfo :: ConnectInfo
  }

newtype Pool = Pool { unPool :: MVar PoolState }
