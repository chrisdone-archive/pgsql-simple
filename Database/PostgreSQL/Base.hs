{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ViewPatterns, NamedFieldPuns, TupleSections #-}

-- | A front-end implementation for the PostgreSQL database protocol
--   version 3.0 (implemented in PostgreSQL 7.4 and later).

module Database.PostgreSQL.Base
  (begin
  ,rollback
  ,commit
  ,query
  ,exec
  ,escapeBS
  ,connect
  ,defaultConnectInfo
  ,close
  ,withDB
  ,withTransaction
  ,newPool
  ,pconnect
  ,withPoolConnection)
  where

import           Database.PostgreSQL.Base.Types

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.CatchIO          (MonadCatchIO)
import qualified Control.Monad.CatchIO          as E
import           Control.Monad.Fix
import           Control.Monad.State            (MonadState,execStateT,modify)
import           Control.Monad.Trans
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Data.ByteString.Lazy.UTF8      as L (toString,fromString)
import           Data.ByteString.UTF8           (toString,fromString)
import           Data.Int
import           Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Monoid
import           Network
import           Prelude
import           System.IO                      hiding (hPutStr)

--------------------------------------------------------------------------------
-- Exported values

-- | Default information for setting up a connection.
--
-- Defaults are as follows:
--
-- * Server on @localhost@
--
-- * User @postgres@
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
                       connectHost = "127.0.0.1"
                     , connectPort = 5432
                     , connectUser = "postgres"
                     , connectPassword = ""
                     , connectDatabase = ""
                     }

-- | Create a new connection pool.
newPool :: MonadIO m
        => ConnectInfo -- ^ Connect info.
        -> m Pool
newPool info = liftIO $ do
  var <- newMVar $ PoolState {
    poolConnections = []
  , poolConnectInfo = info
  }
  return $ Pool var

-- | Connect using the connection pool.
pconnect :: MonadIO m => Pool -> m Connection
pconnect (Pool var) = liftIO $ do
  modifyMVar var $ \state@PoolState{..} -> do
    case poolConnections of
      []           -> do conn <- connect poolConnectInfo
                         return (state,conn)
      (conn:conns) -> return (state { poolConnections = conns },conn)

-- | Restore a connection to the pool.
restore :: MonadIO m => Pool -> Connection -> m ()
restore (Pool var) conn = liftIO $ do
  handle <- readMVar $ connectionHandle conn
  modifyMVar_ var $ \state -> do
    case handle of
      Nothing -> return state
      Just h -> do
        eof <- hIsOpen h
        if eof
           then return state { poolConnections = conn : poolConnections state }
           else return state

-- | Use the connection pool.
withPoolConnection
  :: (MonadCatchIO m,MonadIO m)
  => Pool                 -- ^ The connection pool.
  -> (Connection -> m a) -- ^ Use the connection.
  -> m ()
withPoolConnection pool m = do
  _ <- E.bracket (pconnect pool) (restore pool) m
  return ()

-- | Connect with the given username to the given database. Will throw
--   an exception if it cannot connect.
connect :: MonadIO m => ConnectInfo -> m Connection -- ^ The datase connection.
connect connectInfo@ConnectInfo{..} = liftIO $ withSocketsDo $ do
  var <- newEmptyMVar
  h <- connectTo connectHost (PortNumber $ fromIntegral connectPort)
  hSetBuffering h NoBuffering
  putMVar var $ Just h
  types <- newMVar M.empty
  let conn = Connection var types
  authenticate conn connectInfo
  return conn

-- | Run a an action with a connection and close the connection
--   afterwards (protects against exceptions).
withDB :: (MonadCatchIO m,MonadIO m) => ConnectInfo -> (Connection -> m a) -> m a
withDB connectInfo m = E.bracket (liftIO $ connect connectInfo) (liftIO . close) m

-- | With a transaction, do some action (protects against exceptions).
withTransaction :: (MonadCatchIO m,MonadIO m) => Connection -> m a -> m a
withTransaction conn act = do
  begin conn
  r <- act `E.onException` rollback conn
  commit conn
  return r

-- | Rollback a transaction.
rollback :: (MonadCatchIO m,MonadIO m) => Connection -> m ()
rollback conn = do
  _ <- exec conn (fromString ("ABORT;" :: String))
  return ()

-- | Commit a transaction.
commit :: (MonadCatchIO m,MonadIO m) => Connection -> m ()
commit conn = do
  _ <- exec conn (fromString ("COMMIT;" :: String))
  return ()

-- | Begin a transaction.
begin :: (MonadCatchIO m,MonadIO m) => Connection -> m ()
begin conn = do
  _ <- exec conn (fromString ("BEGIN;" :: String))
  return ()

-- | Close a connection. Can safely be called any number of times.
close :: MonadIO m => Connection -- ^ The connection.
      -> m ()
close Connection{connectionHandle} = liftIO$ do
  modifyMVar_ connectionHandle $ \h -> do
    case h of
      Just h -> hClose h
      Nothing -> return ()
    return Nothing

-- | Run a simple query on a connection.
query :: (MonadCatchIO m)
      => Connection -- ^ The connection.
      -> ByteString              -- ^ The query.
      -> m ([Field],[[Maybe ByteString]])
query conn sql = do
  result <- execQuery conn sql
  case result of
    (_,Just ok) -> return ok
    _           -> return ([],[])

-- | Run a simple query on a connection.
execQuery :: (MonadCatchIO m)
      => Connection -- ^ The connection.
      -> ByteString              -- ^ The query.
      -> m (Integer,Maybe ([Field],[[Maybe ByteString]]))
execQuery conn sql = liftIO $ do
  withConnection conn $ \h -> do
    types <- readMVar $ connectionObjects conn
    Result{..} <- sendQuery types h sql
    case resultType of
      ErrorResponse -> E.throw $ QueryError (fmap L.toString resultError)
      EmptyQueryResponse -> E.throw QueryEmpty
      _             ->
        let tagCount = fromMaybe 0 resultTagRows
        in case resultDesc of
             Just fields -> return $ (tagCount,Just (fields,resultRows))
             Nothing     -> return $ (tagCount,Nothing)

-- | Exec a command.
exec :: (MonadCatchIO m)
     => Connection
     -> ByteString
     -> m Integer
exec conn sql = do
  result <- execQuery conn sql
  case result of
    (ok,_) -> return ok

-- | PostgreSQL protocol version supported by this library.    
protocolVersion :: Int32
protocolVersion = 196608

-- | Escape a string for PostgreSQL.
escape :: String -> String
escape ('\\':cs) = '\\' : '\\' : escape cs
escape ('\'':cs) = '\'' : '\'' : escape cs
escape (c:cs) = c : escape cs
escape [] = []

-- | Escape a string for PostgreSQL.
escapeBS :: ByteString -> ByteString
escapeBS = fromString . escape . toString

--------------------------------------------------------------------------------
-- Authentication

-- | Run the connectInfoentication procedure.
authenticate :: Connection -> ConnectInfo -> IO ()
authenticate conn@Connection{..} connectInfo = do
  withConnection conn $ \h -> do
    sendStartUp h connectInfo
    getConnectInfoResponse h connectInfo
    objects <- objectIds h
    modifyMVar_ connectionObjects (\_ -> return objects)

-- | Send the start-up message.
sendStartUp :: Handle -> ConnectInfo -> IO ()
sendStartUp h ConnectInfo{..} = do
  sendBlock h Nothing $ do
    int32 protocolVersion
    string (fromString "user")     ; string (fromString connectUser)
    string (fromString "database") ; string (fromString connectDatabase)
    zero

-- | Wait for and process the connectInfoentication response from the server.
getConnectInfoResponse :: Handle -> ConnectInfo -> IO ()
getConnectInfoResponse h conninfo = do
  (typ,block) <- getMessage h
  -- TODO: Handle connectInfo failure. Handle information messages that are
  --       sent, maybe store in the connection value for later
  --       inspection.
  case typ of
    AuthenticationOk
      | param == 0 -> waitForReady h
      | param == 3 -> sendPassClearText h conninfo
--      | param == 5 -> sendPassMd5 h conninfo salt
      | otherwise  -> E.throw $ UnsupportedAuthenticationMethod param (show block)
        where param = decode block :: Int32
              _salt = flip runGet block $ do
                        _ <- getInt32
                        getWord8
    
    els -> E.throw $ AuthenticationFailed (show (els,block))

-- | Send the pass as clear text and wait for connect response.
sendPassClearText :: Handle -> ConnectInfo -> IO ()
sendPassClearText h conninfo@ConnectInfo{..} = do
  sendMessage h PasswordMessage $
    string (fromString connectPassword)
  getConnectInfoResponse h conninfo

-- -- | Send the pass as salted MD5 and wait for connect response.
-- sendPassMd5 :: Handle -> ConnectInfo -> Word8 -> IO ()
-- sendPassMd5 h conninfo@ConnectInfo{..} salt = do
--   -- TODO: Need md5 library with salt support.
--   sendMessage h PasswordMessage $
--     string (fromString connectPassword)
--   getConnectInfoResponse h conninfo

--------------------------------------------------------------------------------
-- Initialization

objectIds :: Handle -> IO (Map ObjectId String)
objectIds h = do
    Result{..} <- sendQuery M.empty h q
    case resultType of
      ErrorResponse -> E.throw $ InitializationError "Couldn't get types."
      _ -> return $ M.fromList $ catMaybes $ flip map resultRows $ \row ->
             case map toString $ catMaybes row of
               [typ,readMay -> Just objId] -> Just $ (ObjectId objId,typ)
               _                           -> Nothing

  where q = fromString ("SELECT typname, oid FROM pg_type" :: String)

--------------------------------------------------------------------------------
-- Queries and commands

-- | Send a simple query.
sendQuery :: Map ObjectId String -> Handle -> ByteString -> IO Result
sendQuery types h sql = do
  sendMessage h Query $ string sql
  listener $ \continue -> do
    (typ,block) <- liftIO $ getMessage h
    let setStatus = modify $ \r -> r { resultType = typ }
    case typ of
      ReadyForQuery ->
        modify $ \r -> r { resultRows = reverse (resultRows r) }

      listenPassively -> do
        case listenPassively of
          EmptyQueryResponse -> setStatus
          CommandComplete    -> do setStatus
                                   setCommandTag block
          ErrorResponse -> do
            modify $ \r -> r { resultError = Just block }
            setStatus
          RowDescription -> getRowDesc types block
          DataRow        -> getDataRow block
          _ -> return ()

        continue

  where emptyResponse = Result [] Nothing Nothing [] UnknownMessageType Nothing
        listener m = execStateT (fix m) emptyResponse

-- | CommandComplete returns a ‘tag’ which indicates how many rows were
-- affected, or returned, as a result of the command.
-- See http://developer.postgresql.org/pgdocs/postgres/protocol-message-formats.html
setCommandTag :: MonadState Result m => L.ByteString -> m ()
setCommandTag block = do
  modify $ \r -> r { resultTagRows = rows }
    where rows =
            case tag block of
              ["INSERT",_oid,readMay -> Just rows]         -> return rows
              [cmd,readMay -> Just rows] | cmd `elem` cmds -> return rows
              _                                            -> Nothing
          tag = words . concat . map toString . L.toChunks . runGet getString
          cmds = ["DELETE","UPDATE","SELECT","MOVE","FETCH"]

-- | Update the row description of the result.
getRowDesc :: MonadState Result m => Map ObjectId String -> L.ByteString -> m ()
getRowDesc types block =
  modify $ \r -> r {
    resultDesc = Just (parseFields types (runGet parseMsg block))
  }
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
              return (name,objid,colid,dtype,size,modifier,code)

-- | Parse a row description.
--
-- Parts of the row description are:
--
-- String: The field name.
--
-- Int32: If the field can be identified as a column of a specific
-- table, the object ID of the table; otherwise zero.
--
-- Int16: If the field can be identified as a column of a specific
-- table, the attribute number of the column; otherwise zero.
----
-- Int32: The object ID of the field's data type.
----
-- Int16: The data type size (see pg_type.typlen). Note that negative
-- values denote variable-width types.
----
-- Int32: The type modifier (see pg_attribute.atttypmod). The meaning
-- of the modifier is type-specific.
--
-- Int16: The format code being used for the field. Currently will be
-- zero (text) or one (binary). In a RowDescription returned from the
-- statement variant of Describe, the format code is not yet known and
-- will always be zero.
--
parseFields :: Map ObjectId String
            -> [(L.ByteString,Int32,Int16,Int32,Int16,Int32,Int16)]
            -> [Field]
parseFields types = map parse where
  parse (_fieldName
        ,_ -- parseObjId        -> _objectId
        ,_ -- parseAttrId       -> _attrId
        ,parseType types   -> typ
        ,_ -- parseSize         -> _typeSize
        ,_ -- parseModifier typ -> _typeModifier
        ,parseFormatCode   -> formatCode)
    = Field {
      fieldType = typ
    , fieldFormatCode = formatCode
    }

-- These aren't used (yet).

-- -- | Parse an object ID. 0 means no object.
-- parseObjId :: Int32 -> Maybe ObjectId
-- parseObjId 0 = Nothing
-- parseObjId n = Just (ObjectId n)

-- -- | Parse an attribute ID. 0 means no object.
-- parseAttrId :: Int16 -> Maybe ObjectId
-- parseAttrId 0 = Nothing
-- parseAttrId n = Just (ObjectId $ fromIntegral n)

-- | Parse a number into a type.
parseType :: Map ObjectId String -> Int32 -> Type
parseType types objId =
  case M.lookup (ObjectId objId) types of
    Just name -> case typeFromName name of
                   Just typ -> typ
                   Nothing -> error $ "parseType: Unknown type: " ++ show name
    _ -> error $ "parseType: Unable to parse type: " ++ show objId

typeFromName :: String -> Maybe Type
typeFromName = flip lookup fieldTypes

fieldTypes :: [(String, Type)]
fieldTypes =
  [("bool",Boolean)
  ,("int2",Short)
  ,("integer",Long)
  ,("int",Long)
  ,("int4",Long)
  ,("int8",LongLong)
  ,("timestamptz",TimestampWithZone)
  ,("varchar",CharVarying)
  ,("text",Text)]

-- This isn't used yet.
-- | Parse a type's size.
-- parseSize :: Int16 -> Size
-- parseSize (-1) = Varying
-- parseSize n    = Size n

-- This isn't used yet.
-- -- | Parse a type-specific modifier.
-- parseModifier :: Type -> Int32 -> Maybe Modifier
-- parseModifier _typ _modifier = Nothing

-- | Parse a format code (text or binary).
parseFormatCode :: Int16 -> FormatCode
parseFormatCode 1 = BinaryCode
parseFormatCode _ = TextCode

-- | Add a data row to the response.
getDataRow :: MonadState Result m => L.ByteString -> m ()
getDataRow block =
  modify $ \r -> r { resultRows = runGet parseMsg block : resultRows r }
    where parseMsg = do
            values :: Int16 <- getInt16
            forM [1..values] $ \_ -> do
              size <- getInt32
              if size == -1
                 then return Nothing
                 else do v <- getByteString (fromIntegral size)
                         return (Just v)

-- TODO:
-- getNotice :: MonadState Result m => L.ByteString -> m ()
-- getNotice block =
--   return ()
--  modify $ \r -> r { responseNotices = runGet parseMsg block : responseNotices r }
--    where parseMsg = return ""

typeFromChar :: Char -> Maybe MessageType
typeFromChar c = lookup c types

charFromType :: MessageType -> Maybe Char
charFromType typ = fmap fst $ find ((==typ).snd) types

types :: [(Char, MessageType)]
types = [('C',CommandComplete)
        ,('T',RowDescription)
        ,('D',DataRow)
        ,('I',EmptyQueryResponse)
        ,('E',ErrorResponse)
        ,('Z',ReadyForQuery)
        ,('N',NoticeResponse)
        ,('R',AuthenticationOk)
        ,('Q',Query)
        ,('p',PasswordMessage)]

-- | Blocks until receives ReadyForQuery.
waitForReady :: Handle -> IO ()
waitForReady h = loop where
  loop = do
  (typ,block) <- getMessage h
  case typ of
    ErrorResponse -> E.throw $ GeneralError $ show block
    ReadyForQuery | decode block == 'I' -> return ()
    _                                   -> loop

--------------------------------------------------------------------------------
-- Connections

-- | Atomically perform an action with the database handle, if there is one.
withConnection :: Connection -> (Handle -> IO a) -> IO a
withConnection Connection{..} m = do
  withMVar connectionHandle $ \h -> do
    case h of
      Just h -> m h
      -- TODO: Use extensible exceptions.
      Nothing -> E.throw ConnectionLost

-- | Send a block of bytes on a handle, prepending the message type
--   and complete length.
sendMessage :: Handle -> MessageType -> Put -> IO ()
sendMessage h typ output =
  case charFromType typ of
    Just char -> sendBlock h (Just char) output
    Nothing   -> error $ "sendMessage: Bad message type " ++ show typ

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
string :: B.ByteString -> Put
string s = do putByteString s; zero

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

readMay :: Read a => String -> Maybe a
readMay x = case reads x of
              [(v,"")] -> return v
              _        -> Nothing
