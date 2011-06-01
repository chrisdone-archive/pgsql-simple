{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Database.PostgreSQL.Simple.QueryResults
-- Copyright:   (c) 2011 Chris Done, 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Chris Done <chrisdone@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'QueryResults' typeclass, for converting a row of results
-- returned by a SQL query into a more useful Haskell representation.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.

module Database.PostgreSQL.Simple.QueryResults
    ( QueryResults(..)
    , convertError
    ) where

import Control.Exception (throw)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Base.Types (Field(fieldType))
import Database.PostgreSQL.Simple.Result (ResultError(..), Result(..))
import Database.PostgreSQL.Simple.Types (Only(..))

-- | A collection type that can be converted from a list of strings.
--
-- Instances should use the 'convert' method of the 'Result' class
-- to perform conversion of each element of the collection.
--
-- This example instance demonstrates how to convert a two-column row
-- into a Haskell pair. Each field in the metadata is paired up with
-- each value from the row, and the two are passed to 'convert'.
--
-- @
-- instance ('Result' a, 'Result' b) => 'QueryResults' (a,b) where
--     'convertResults' [fa,fb] [va,vb] = (a,b)
--         where !a = 'convert' fa va
--               !b = 'convert' fb vb
--     'convertResults' fs vs  = 'convertError' fs vs
-- @
--
-- Notice that this instance evaluates each element to WHNF before
-- constructing the pair. By doing this, we guarantee two important
-- properties:
--
-- * Keep resource usage under control by preventing the construction
--   of potentially long-lived thunks.
--
-- * Ensure that any 'ResultError' that might arise is thrown
--   immediately, rather than some place later in application code
--   that cannot handle it.
--
-- You can also declare Haskell types of your own to be instances of
-- 'QueryResults'.
--
-- @
-- data User { firstName :: String, lastName :: String }

-- instance 'QueryResults' User where
--    'convertResults' [fa,fb] [va,vb] = User a b
--        where !a = 'convert' fa va
--              !b = 'convert' fb vb
--    'convertResults' fs vs  = 'convertError' fs vs
-- @

class QueryResults a where
    convertResults :: [Field] -> [Maybe ByteString] -> a
    -- ^ Convert values from a row into a Haskell collection.
    --
    -- This function will throw a 'ResultError' if conversion of the
    -- collection fails.

instance (Result a) => QueryResults (Only a) where
    convertResults [fa] [va] = Only a
        where !a = convert fa va
    convertResults fs vs  = convertError fs vs 1

instance (Result a, Result b) => QueryResults (a,b) where
    convertResults [fa,fb] [va,vb] = (a,b)
        where !a = convert fa va; !b = convert fb vb
    convertResults fs vs  = convertError fs vs 2

instance (Result a, Result b, Result c) => QueryResults (a,b,c) where
    convertResults [fa,fb,fc] [va,vb,vc] = (a,b,c)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
    convertResults fs vs  = convertError fs vs 3

instance (Result a, Result b, Result c, Result d) =>
    QueryResults (a,b,c,d) where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = (a,b,c,d)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd
    convertResults fs vs  = convertError fs vs 4

instance (Result a, Result b, Result c, Result d, Result e) =>
    QueryResults (a,b,c,d,e) where
    convertResults [fa,fb,fc,fd,fe] [va,vb,vc,vd,ve] = (a,b,c,d,e)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve
    convertResults fs vs  = convertError fs vs 5

instance (Result a, Result b, Result c, Result d, Result e, Result f) =>
    QueryResults (a,b,c,d,e,f) where
    convertResults [fa,fb,fc,fd,fe,ff] [va,vb,vc,vd,ve,vf] = (a,b,c,d,e,f)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
    convertResults fs vs  = convertError fs vs 6

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g) =>
    QueryResults (a,b,c,d,e,f,g) where
    convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] =
        (a,b,c,d,e,f,g)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg
    convertResults fs vs  = convertError fs vs 7

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h) =>
    QueryResults (a,b,c,d,e,f,g,h) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh] [va,vb,vc,vd,ve,vf,vg,vh] =
        (a,b,c,d,e,f,g,h)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh
    convertResults fs vs  = convertError fs vs 8

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i) =>
    QueryResults (a,b,c,d,e,f,g,h,i) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi] [va,vb,vc,vd,ve,vf,vg,vh,vi] =
        (a,b,c,d,e,f,g,h,i)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
    convertResults fs vs  = convertError fs vs 9

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj] =
        (a,b,c,d,e,f,g,h,i,j)
        where !a = convert fa va; !b = convert fb vb; !c = convert fc vc
              !d = convert fd vd; !e = convert fe ve; !f = convert ff vf
              !g = convert fg vg; !h = convert fh vh; !i = convert fi vi
              !j = convert fj vj
    convertResults fs vs  = convertError fs vs 10

-- | Throw a 'ConversionFailed' exception, indicating a mismatch
-- between the number of columns in the 'Field' and row, and the
-- number in the collection to be converted to.
convertError :: [Field] -> [Maybe ByteString] -> Int -> a
convertError fs vs n = throw $ ConversionFailed
    (show (length fs) ++ " values: " ++ show (zip (map fieldType fs)
                                                  (map (fmap ellipsis) vs)))
    (show n ++ " slots in target type")
    "mismatch between number of columns to convert and number in target type"

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs
