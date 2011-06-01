{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

-- | A front-end implementation for the PostgreSQL database protocol
--   version 3.0 (implemented in PostgreSQL 7.4 and later).

module Database.PostgreSQL.Base where

import           Database.PostgreSQL.Base.Types

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State (MonadState,execStateT,modify)
import           Control.Monad.Fix
import           Control.Monad.CatchIO     (MonadCatchIO -- ,onException
 )
import qualified Control.Monad.CatchIO     as E
import           Control.Monad.Trans
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString           as B
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as L
-- import qualified Data.ByteString.Lazy.UTF8 as UTF8
-- import           Data.ByteString.UTF8      (toString)
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Network
import           Prelude
import           System.IO                 hiding (hPutStr)

-- FIXME: Proper escape function.
escape :: String -> String
escape str = undefined

-- FIXME:
insertID :: Connection -> IO Word64
insertID _ = return 0

-- FIXME:
-- | Turn autocommit on or off.
--
-- By default, PostgreSQL runs with autocommit mode enabled. In this
-- mode, as soon as you modify a table, PostgreSQL stores your
-- modification permanently.
autocommit :: Connection -> Bool -> IO ()
autocommit conn onOff = return () -- withConnection conn $ \ptr ->
 --   mysql_autocommit ptr b >>= check "autocommit" conn
 -- where b = if onOff then 1 else 0

--------------------------------------------------------------------------------
-- Exported values

-- | Default information for setting up a connection.
--
-- Defaults are as follows:
--
-- * Server on @localhost@
--
-- * User @root@
--
-- * No password
--
-- * Database @test@
--
-- * Character set @utf8@
--
-- Use as in the following example:
--
-- > connect defaultConnectInfo { connectHost = "db.example.com" }
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo {
                       connectHost = "localhost"
                     , connectPort = 5432
                     , connectUser = "root"
                     , connectPassword = ""
                     , connectDatabase = "test"
                     }

-- | Connect with the given username to the given database. Will throw
--   an exception if it cannot connect.
connect :: MonadIO m => ConnectInfo -> m Connection -- ^ The datase connection.
connect connectInfo@ConnectInfo{..} = liftIO $ withSocketsDo $ do
  var <- newEmptyMVar
  h <- connectTo connectHost (PortNumber $ fromIntegral connectPort)
  hSetBuffering h NoBuffering
  putMVar var $ Just h
  let conn = Connection var
  authenticate conn connectInfo
  return conn

withDB :: (MonadCatchIO m,MonadIO m) => ConnectInfo -> (Connection -> m a) -> m a
withDB connectInfo m = E.bracket (liftIO $ connect connectInfo) (liftIO . close) m

rollback :: (MonadCatchIO m,MonadIO m) => Connection -> m ()
rollback conn = do
  query conn (fromString ("ABORT;" :: String))
  return ()

commit :: (MonadCatchIO m,MonadIO m) => Connection -> m ()
commit conn = do
  query conn (fromString ("COMMIT;" :: String))
  return ()

-- | Close a connection. Can safely be called any number of times.
close :: MonadIO m => Connection -- ^ The connection.
      -> m ()
close (Connection v) = liftIO$ do
  modifyMVar_ v $ \h -> do
    case h of
      Just h -> hClose h
      Nothing -> return ()
    return Nothing

-- | Run a simple query on a connection.
query :: MonadIO m => Connection -- ^ The connection.
      -> ByteString                  -- ^ The query.
      -> m [a]
query conn sql = liftIO $ do
  withConnection conn $ \h -> do
    Result{..} <- sendQuery h sql
    case resultType of
      ErrorResponse -> error "TODO: query.ErrorResponse"
      _             -> return $ [] -- resultRows -- FIXME:

-- | PostgreSQL protocol version supported by this library.    
protocolVersion :: Int32
protocolVersion = 196608

--------------------------------------------------------------------------------
-- Authentication

-- | Run the connectInfoentication procedure.
authenticate :: Connection -> ConnectInfo -> IO ()
authenticate conn connectInfo = do
  withConnection conn $ \h -> do
    sendStartUp h connectInfo
    getConnectInfoResponse h
    return ()

-- | Send the start-up message.
sendStartUp :: Handle -> ConnectInfo -> IO ()
sendStartUp h ConnectInfo{..} = do
  sendBlock h Nothing $ do
    int32 protocolVersion
    string (fromString "user")     ; string (fromString connectUser)
    string (fromString "database") ; string (fromString connectDatabase)
    zero

-- | Wait for and process the connectInfoentication response from the server.
getConnectInfoResponse :: Handle -> IO ()
getConnectInfoResponse h = do
  (typ,block) <- getMessage h
  case typ of
    AuthenticationOk | param == 0 -> waitForReady h
        where param = decode block :: Int32
    -- TODO: Handle connectInfo failure. Handle information messages that are
    --       sent, maybe store in the connection value for later
    --       inspection.
    _ -> return ()

--------------------------------------------------------------------------------
-- Initialization

typeObjectIds :: Connection -> IO [(String,Int32)]
typeObjectIds conn = do
  withConnection conn $ \h -> do
    Result{..} <- sendQuery h (fromString ("SELECT typname, oid FROM pg_type" :: String))
    case resultType of
      ErrorResponse -> return [] -- TODO: Throw an error in some nice way.
        -- FIXME:
      -- _ -> return $ catMaybes $ flip map resultRows $ \row ->
      --        case map toString $ catMaybes row of
      --          [typ,objId] -> Just $ (typ,read objId)
      --          _           -> Nothing

--------------------------------------------------------------------------------
-- Queries and commands

-- | Send a simple query.
sendQuery :: Handle -> ByteString -> IO Result
sendQuery h sql = do
  sendMessage h Query $ string sql
  listener $ \continue -> do
    (typ,block) <- liftIO $ getMessage h
    let done = modify $ \r -> r { resultType = typ }
    case typ of
      CommandComplete    -> done
      EmptyQueryResponse -> done
      ReadyForQuery      -> done
      ErrorResponse      -> do
        modify $ \r -> r { resultError = Just block }
        done

      listenFor -> do
        case listenFor of
          RowDescription -> getRowDesc block
          DataRow        -> getDataRow block
          NoticeResponse -> getNotice block
          _ -> return ()
        continue

  where emptyResponse = Result Nothing Nothing [] UnknownMessageType
        listener m = execStateT (fix m) emptyResponse

-- | Update the row description of the result.
getRowDesc :: MonadState Result m => L.ByteString -> m ()
getRowDesc block =
  modify $ \r -> r { resultDesc = Just (runGet parseMsg block) }
    where parseMsg = do
            fieldCount :: Int16 <- getInt16
            forM [1..fieldCount] $ \_ -> do
              name <- getString
              objid <- getInt32
              colid <- getInt16
              dtype <- getInt32
              size <- getInt16
              modifier <- getInt32
              code <- getInt16
              return (name,objid,colid,dtype,size,modifier)

-- | Add a data row to the response.
getDataRow :: MonadState Result m => L.ByteString -> m ()
getDataRow block =
  return ()
--  modify $ \r -> r { resultRows = runGet parseMsg block : resultRows r }
    where parseMsg = do
            values :: Int16 <- getInt16
            forM [1..values] $ \_ -> do
              size <- getInt32
              if size == -1
                 then return Nothing
                 else do v <- getByteString (fromIntegral size)
                         return (Just v)

-- TODO:
getNotice :: MonadState Result m => L.ByteString -> m ()
getNotice block =
  return ()
--  modify $ \r -> r { responseNotices = runGet parseMsg block : responseNotices r }
--    where parseMsg = return ""

typeFromChar :: Char -> Maybe MessageType
typeFromChar c = lookup c types

charFromType :: MessageType -> Maybe Char
charFromType typ = fmap fst $ find ((==typ).snd) types

types = [('C',CommandComplete)
        ,('T',RowDescription)
        ,('D',DataRow)
        ,('I',EmptyQueryResponse)
        ,('E',ErrorResponse)
        ,('Z',ReadyForQuery)
        ,('N',NoticeResponse)
        ,('R',AuthenticationOk)
        ,('Q',Query)]

-- | Blocks until receives ReadyForQuery.
waitForReady :: Handle -> IO ()
waitForReady h = loop where
  loop = do
  (typ,block) <- getMessage h
  case typ of
    ReadyForQuery | decode block == 'I' -> return ()
    diff                      -> loop

--------------------------------------------------------------------------------
-- Connections

-- | Atomically perform an action with the database handle, if there is one.
withConnection :: Connection -> (Handle -> IO a) -> IO a
withConnection Connection{..} m = do
  withMVar connectionHandle $ \h -> do
    case h of
      Just h -> m h
      -- TODO: Use extensible exceptions.
      Nothing -> error "Database.PostgreSQL.withConnection: Connection is lost."

-- | Send a block of bytes on a handle, prepending the message type
--   and complete length.
sendMessage :: Handle -> MessageType -> Put -> IO ()
sendMessage h typ output =
  case charFromType typ of
    Just char -> sendBlock h (Just char) output
    Nothing   -> return () -- TODO: Possibly throw an error. Or just ignore?

-- | Send a block of bytes on a handle, prepending the complete length.
sendBlock :: Handle -> Maybe Char -> Put -> IO ()
sendBlock h typ output = do
  L.hPutStr h bytes
    where bytes = start `mappend` out
          start = runPut $ do
            maybe (return ()) (put . toByte) typ
            int32 $ fromIntegral int32Size +
                    fromIntegral (L.length out)
          out = runPut output
          toByte c = fromIntegral (fromEnum c) :: Word8

-- | Get a message (block) from the stream.
getMessage :: Handle -> IO (MessageType,L.ByteString)
getMessage h = do
  messageType <- L.hGet h 1
  blockLength <- L.hGet h int32Size
  let typ = decode messageType
      rest = fromIntegral (decode blockLength :: Int32) - int32Size
  block <- L.hGet h rest
  return (maybe UnknownMessageType id $ typeFromChar typ,block)

--------------------------------------------------------------------------------
-- Binary input/output

-- | Put a Haskell string, encoding it to UTF-8, and null-terminating it.
--   TODO: Make this not terrible.
string :: ByteString -> Put
string s = do put s; zero
fromString = const (undefined :: ByteString)
-- FIXME:
toString = const ""

-- | Put a Haskell 32-bit integer.
int32 :: Int32 -> Put
int32 = put

-- | Put zero-byte terminator.
zero :: Put
zero = put (0 :: Word8)

-- | To avoid magic numbers, size of a 32-bit integer in bytes.
int32Size :: Int
int32Size = 4

getInt16 :: Get Int16
getInt16 = get

getInt32 :: Get Int32
getInt32 = get

getString :: Get L.ByteString
getString = getLazyByteStringNul
