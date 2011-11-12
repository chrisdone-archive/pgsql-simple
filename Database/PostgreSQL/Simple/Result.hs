{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances #-}

-- |
-- Module:      Database.PostgreSQL.Simpe.QueryResults
-- Copyright:   (c) 2011 Chris Done, 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Chris Done <chrisdone@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'Result' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- MySQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the MySQL 'Long' type
-- because it can represent a 'Long' exactly. On the other hand, since
-- a 'Double' might lose precision if representing a 'LongLong', the
-- two are /not/ considered compatible.

module Database.PostgreSQL.Simple.Result
    ( Result(..)
    , ResultError(..)
    ) where

#include "MachDeps.h"

import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import Control.Exception (Exception, throw)
import Data.Attoparsec.Char8 hiding (Result)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (TimeOfDay,LocalTime,ZonedTime,makeTimeOfDayValid)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word, Word16, Word32, Word64)
import Database.PostgreSQL.Base.Types (Field(..),Type(..),FormatCode(..))
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: String
                                , errHaskellType :: String
                                , errMessage :: String }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: String
                                  , errHaskellType :: String
                                  , errMessage :: String }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: String
                                    , errHaskellType :: String
                                    , errMessage :: String }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

-- | A type that may be converted from a SQL type.
class Result a where
    convert :: Field -> Maybe ByteString -> a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Throws a 'ResultError' if conversion fails.

instance (Result a) => Result (Maybe a) where
    convert _ Nothing = Nothing
    convert f bs      = Just (convert f bs)

instance Result Bool where
    convert _ (Just t)
      | str == "t" = True
      | str == "f" = False
     where str = B8.unpack t
    convert f _ = conversionFailed f "Bool" "could not parse"

instance Result Int16 where
    convert = atto ok16 $ signed decimal

instance Result Int32 where
    convert = atto ok32 $ signed decimal

instance Result Int where
    convert = atto okWord $ signed decimal

instance Result Int64 where
    convert = atto ok64 $ signed decimal

instance Result Integer where
    convert = atto ok64 $ signed decimal

instance Result Word16 where
    convert = atto ok16 decimal

instance Result Word32 where
    convert = atto ok32 decimal

instance Result Word where
    convert = atto okWord decimal

instance Result Word64 where
    convert = atto ok64 decimal

instance Result Float where
    convert = atto ok ((fromRational . toRational) <$> double)
        where ok = mkCompats [Real,Short,Long]

instance Result Double where
    convert = atto ok double
        where ok = mkCompats [Real,DoublePrecision,Short,Long]

instance Result (Ratio Integer) where
    convert = atto ok rational
        where ok = mkCompats [Decimal,Numeric,Real,DoublePrecision]

instance Result SB.ByteString where
    convert f = doConvert f okText $ id

instance Result LB.ByteString where
    convert f = LB.fromChunks . (:[]) . convert f

instance Result ST.Text where
    convert f | isText f  = doConvert f okText $ ST.decodeUtf8
              | otherwise = incompatible f (typeOf ST.empty)
                            "attempt to mix binary and text"

instance Result LT.Text where
    convert f = LT.fromStrict . convert f

instance Result [Char] where
    convert f = ST.unpack . convert f

instance Result LocalTime where
    convert f = doConvert f ok $ \bs ->
                case parseLocalTime (B8.unpack bs) of
                  Just t -> t
                  Nothing -> conversionFailed f "UTCTime" "could not parse"
        where ok = mkCompats [TimestampWithZone]

parseLocalTime :: String -> Maybe LocalTime
parseLocalTime s =
  parseTime defaultTimeLocale "%F %T%Q" s <|>
  parseTime defaultTimeLocale "%F %T%Q%z" (s ++ "00")

instance Result ZonedTime where
    convert f = doConvert f ok $ \bs ->
                case parseZonedTime (B8.unpack bs) of
                  Just t -> t
                  Nothing -> conversionFailed f "UTCTime" "could not parse"
        where ok = mkCompats [TimestampWithZone]

parseZonedTime :: String -> Maybe ZonedTime
parseZonedTime s =
  parseTime defaultTimeLocale "%F %T%Q%z" (s ++ "00")

instance Result UTCTime where
    convert f = doConvert f ok $ \bs ->
                case parseTime defaultTimeLocale "%F %T%Q" (B8.unpack bs) of
                  Just t -> t
                  Nothing -> conversionFailed f "UTCTime" "could not parse"
        where ok = mkCompats [Timestamp]

instance Result Day where
    convert f = flip (atto ok) f $ date
        where ok = mkCompats [Date]
              date = fromGregorian <$> (decimal <* char '-')
                                   <*> (decimal <* char '-')
                                   <*> decimal

instance Result TimeOfDay where
    convert f = flip (atto ok) f $ do
                hours <- decimal <* char ':'
                mins <- decimal <* char ':'
                secs <- decimal :: Parser Int
                case makeTimeOfDayValid hours mins (fromIntegral secs) of
                  Just t -> return t
                  _      -> conversionFailed f "TimeOfDay" "could not parse"
        where ok = mkCompats [Time]

isText :: Field -> Bool
isText f = fieldFormatCode f == TextCode

newtype Compat = Compat Word32

mkCompats :: [Type] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: Type -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0

okText, ok16, ok32, ok64, okWord :: Compat
okText = mkCompats [CharVarying,Characters,Text]
ok16 = mkCompats [Short]
ok32 = mkCompats [Short,Long]
ok64 = mkCompats [Short,Long,LongLong]
#if WORD_SIZE_IN_BITS < 64
okWord = ok32
#else
okWord = ok64
#endif

doConvert :: (Typeable a) =>
             Field -> Compat -> (ByteString -> a) -> Maybe ByteString -> a
doConvert f types cvt (Just bs)
    | mkCompat (fieldType f) `compat` types = cvt bs
    | otherwise = incompatible f (typeOf (cvt undefined)) "types incompatible"
doConvert f _ cvt _ = throw $ UnexpectedNull (show (fieldType f))
                              (show (typeOf (cvt undefined))) ""

incompatible :: Field -> TypeRep -> String -> a
incompatible f r = throw . Incompatible (show (fieldType f)) (show r)

conversionFailed :: Field -> String -> String -> a
conversionFailed f s = throw . ConversionFailed (show (fieldType f)) s

atto :: (Typeable a) => Compat -> Parser a -> Field -> Maybe ByteString -> a
atto types p0 f = doConvert f types $ go (error "atto") p0
  where
    go :: (Typeable a) => a -> Parser a -> ByteString -> a
    go dummy p s =
        case parseOnly p s of
          Left err -> conversionFailed f (show (typeOf dummy)) err
          Right v  -> v
