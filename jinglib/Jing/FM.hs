{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Jing.FM where

import Prelude hiding (id)
import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Data
import Data.Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP
import Network.URI

-- | Playlist Item.
data PlsItem = PlsItem 
    { abid :: Int       -- album id
    , aid  :: Int       -- artist id
    , an   :: String    -- album name
    , atn  :: String    -- artist name
    , d    :: String
    , fid  :: String
    , fs   :: Int       -- file size
    , mid  :: String
    , n    :: String    -- song name
    , tid  :: Int
    --, y    :: Bool
    } deriving (Data,Typeable,Show)

data Res = Res
    { items :: [PlsItem]
    } deriving (Data,Typeable,Show)

data Resp = Resp
    { result :: Res
    } deriving (Data,Typeable,Show)

data Usr = Usr
    { id :: Int
    , nick :: String
    } deriving (Data,Typeable,Show)

data SessionResult = SessionResult
    { usr :: Usr
    } deriving (Data,Typeable,Show)

-- | Every jingRequest needs the token_header.
data Token = Token
    { jingAToken :: String
    , jingRToken :: String
    , jingUid    :: String
    , jingNick   :: String 
    } deriving (Show)

data ReqSuccess = ReqSuccess { success :: Bool } deriving Show

instance FromJSON ReqSuccess where
    parseJSON (Object v) = ReqSuccess <$>
                           v .: "success"

    parseJSON _          = mzero

-- | Provided valid email/password, return (Just Token)
createSession :: String -> String -> IO (Maybe Token)
createSession email pwd = do
    rsp <- simpleHTTP req
    json <- getResponseBody rsp
    let reqSuccess = decode $ C.pack json :: Maybe ReqSuccess
    if success $ fromJust reqSuccess 
       then do
            let Right resp = rsp
                h = rspHeaders resp
                aToken = fromJust $ lookupHeader aHdr h
                rToken = fromJust $ lookupHeader rHdr h
                d = decode $ C.pack json :: Maybe (Map String Value)
                r = fromJust d ! "result"
                u = G.decode (encode r) :: Maybe SessionResult
            return $ Just Token { jingAToken = aToken
                                , jingRToken = rToken
                                , jingUid    = show $ id $ usr $ fromJust u
                                , jingNick   = nick $ usr $ fromJust u
                                }
       else return Nothing
  where
    param = [ ("email", email) , ("pwd", pwd) ]
    req = postRequestWithBody "http://jing.fm/api/v1/sessions/create" 
                              "application/x-www-form-urlencoded" 
                              (urlEncodeVars param)
    aHdr = HdrCustom "Jing-A-Token-Header"
    rHdr = HdrCustom "Jing-R-Token-Header"

getPlaylist :: String -> ReaderT Token IO [PlsItem]
getPlaylist keywords = do
    tok <- ask
    let uid = jingUid tok
        param = [ ("q", keywords)
                , ("ps", "5")
                , ("st", "0")
                , ("u", uid)
                , ("tid", "0")
                , ("mt", "")
                , ("ss", "true")
                ]
    json <- jingRequest "/search/jing/fetch_pls?" param
    let parsed = G.decode $ C.pack json :: Maybe Resp
        it = fmap items $ fmap result parsed
    return (fromJust it)

getSongUrl :: String -> ReaderT Token IO String
getSongUrl mid = do
    let param = [("type", "NO"), ("mid", mid)]
    json <- jingRequest "/media/song/surl?" param
    let parsed = decode $ C.pack json :: Maybe (Map String Value)
    let (String surl) = (fromJust parsed) ! "result"
    return $ T.unpack surl

postHeard :: Token -> String -> IO String
postHeard tok tid = do
    let uid = jingUid tok
        param = [("uid", uid), ("tid", tid)]
    json <- runReaderT (jingRequest "/music/post_heard_song?" param) tok
    --putStrLn json
    return json

postLove param = jingRequest path param
  where path = "/music/post_love_song?"

postHate param = jingRequest path param
  where path = "/music/post_hate_song?"

jingRequest :: String -> [(String, String)] -> ReaderT Token IO String
jingRequest path param = do
    tok <- ask
    let tokenA = mkHeader aHdr $ jingAToken tok
        tokenR = mkHeader rHdr $ jingRToken tok
    let req = Request { rqURI = uri, rqMethod = POST, rqHeaders = [tokenA, tokenR], rqBody = "" } 
    rsp <- lift $ simpleHTTP req
    liftIO $ getResponseBody rsp
  where
    url = "http://jing.fm/api/v1" ++ path ++ urlEncodeVars param
    uri = fromJust $ parseURI url
    aHdr = HdrCustom "Jing-A-Token-Header"
    rHdr = HdrCustom "Jing-R-Token-Header"

