{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Jing.FM where

import Prelude hiding (id)
import Control.Applicative
import Control.Monad (mzero)
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
    , b    :: String
    , d    :: String
    , fid  :: String
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
    { jingHeader :: [Header]
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
                tokenA = mkHeader aHdr aToken
                tokenR = mkHeader rHdr rToken
            return $ Just Token { jingHeader = [tokenA, tokenR]
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

getPlaylist :: Token -> String -> IO [PlsItem]
getPlaylist tok keywords = do
    json <- jingRequest tok "/search/jing/fetch_pls?" param
    let parsed = G.decode $ C.pack json :: Maybe Resp
        it = fmap items $ fmap result parsed
    return (fromJust it)
  where
    uid = jingUid tok
    param = [ ("q", keywords)
            , ("ps", "5")
            , ("st", "0")
            , ("u", uid)
            , ("tid", "0")
            , ("mt", "")
            , ("ss", "true")
            ]

getSongUrl :: Token -> String -> IO String
getSongUrl tok mid = do
    let param = [("type", "NO"), ("mid", mid)]
    json <- jingRequest tok "/media/song/surl?" param
    let parsed = decode $ C.pack json :: Maybe (Map String Value)
    let (String surl) = (fromJust parsed) ! "result"
    return $ T.unpack surl

postHeard :: Token -> String -> IO String
postHeard tok tid = do
    json <- jingRequest tok "/music/post_heard_song?" param    
    --putStrLn json
    return json
  where
    uid = jingUid tok
    param = [("uid", uid), ("tid", tid)]

postLove tok param = jingRequest tok path param
  where path = "/music/post_love_song?"

postHate tok param = jingRequest tok path param
  where path = "/music/post_hate_song?"

jingRequest :: Token -> String -> [(String, String)] -> IO String
jingRequest tok path param = do
    rsp <- simpleHTTP req
    getResponseBody rsp
  where
    url = "http://jing.fm/api/v1" ++ path ++ urlEncodeVars param
    uri = fromJust $ parseURI url
    h = jingHeader tok
    req = Request { rqURI = uri, rqMethod = POST, rqHeaders = h, rqBody = "" } 



