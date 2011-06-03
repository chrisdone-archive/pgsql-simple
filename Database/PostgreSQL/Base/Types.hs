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
  ,ObjectId(..))
  where

import Control.Concurrent.MVar (MVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Typeable
import Data.Word
import System.IO (Handle)
import Data.Map (Map)

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
  | UnknownMessageType
    deriving (Show,Eq)

-- | A field description.
data Field = Field {
    fieldType :: Type
   ,fieldFormatCode :: FormatCode
  } deriving Show

data Type = 
  -- These types ought to properly match their corresponding
  -- size in the database.
    Short      -- ^ 2 bytes, small-range integer
  | Long       -- ^ 4 bytes, usual choice for integer
  | LongLong   -- ^ 8 bytes	large-range integer
  
  -- -- TODO: This choice for Decimal seems, pardon me,
  -- -- rational, as the precision ought to be
  -- -- infinite. However, I'm not confident in the choice.
  | Decimal -- ^ variable, user-specified precision, exact, no limit
  | Numeric -- ^ variable, user-specified precision, exact, no limit
  
  -- -- TODO: For the IEEE floating points, use isNaN and
  -- -- isInfinite.
  -- -- <http://www.postgresql.org/docs/current/static/datatype-numeric.html>
  | Real             -- ^ 4 bytes, variable-precision, inexact
  | DoublePrecision -- ^ 8 bytes, variable-precision, inexact
  -- --
  -- | Serial Int32    -- ^ 4 bytes, autoincrementing integer
  -- | BigSerial Int64 -- ^ 8 bytes, large autoincrementing integer

  -- -- TODO: Is Money a double?
  -- | Money Double -- ^ 8 bytes, currency amount.
  -- -- See <http://www.postgresql.org/docs/current/static/datatype-money.html> for
  -- -- more information.

  | CharVarying -- ^ character varying(n), varchar(n), variable-length
  | Characters  -- ^ character(n), char(n), fixed-length
  | Text        -- ^ text, variable unlimited length
              -- 
              -- Lazy. Decoded from UTF-8 into Haskell native encoding.
 --  | Bytes ByteString   -- ^ 1 or 4 bytes plus the actual binary string
  -- 
  -- See <http://www.postgresql.org/docs/current/static/datatype-binary.html>
  -- for more information on this type. Strict.

  | Boolean -- ^ boolean, 1 byte, state of true or false

  | Timestamp -- ^ timestamp /without/ time zone
              -- 
              -- More information about PostgreSQL’s dates here:
              -- <http://www.postgresql.org/docs/current/static/datatype-datetime.html>
  | TimestampWithZone -- ^ timestamp /with/ time zone
  | Date              -- ^ date, 4 bytes	julian day
  | Time              -- ^ 8 bytes, time of day (no date)
  -- | ZonedTime ZonedTime         -- ^ 12 bytes, times of day only, with time zone
  -- | Interval DiffTime           -- ^ 12 bytes	time interval

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
