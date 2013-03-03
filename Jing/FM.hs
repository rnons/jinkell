{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Jing.FM where

import Prelude hiding (id)
import Control.Applicative
import Control.Concurrent                                                       
import Control.Monad
import Data.Data
import Data.Typeable
import Data.Aeson
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map
import Data.Maybe (fromJust)
import Network.HTTP
import Network.URI
import Jing.FM.Player.Jinkell.State

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

createSession email pwd = do
    let param = [ ("email", email)
                , ("pwd", pwd)
                ]
    let req = postRequestWithBody "http://jing.fm/api/v1/sessions/create" 
                                  "application/x-www-form-urlencoded" 
                                  (urlEncodeVars param)
    rsp <- simpleHTTP req
    json <- getResponseBody rsp
    let Right resp = rsp
        h = rspHeaders resp
        aToken = fromJust $ lookupHeader (HdrCustom "Jing-A-Token-Header") h
        rToken = fromJust $ lookupHeader (HdrCustom "Jing-R-Token-Header") h
        d = decode $ C.pack json :: Maybe (Map String Value)
        r = fromJust d ! "result"
        u = G.decode (encode r) :: Maybe SessionResult

    print $ "Welcome back, " ++ (nick $ usr $ fromJust u)
    putStrLn "用大白话描述出你想听的音乐"
    let header1 = mkHeader (HdrCustom "Jing-A-Token-Header") aToken
    let header2 = mkHeader (HdrCustom "Jing-R-Token-Header") rToken
    mheader <- newMVar [header1, header2]
    silentlyModifyST $ \st -> st { st_uid = show $ id $ usr $ fromJust u
                                 , st_nick = nick $ usr $ fromJust u
                                 , headers = mheader
                                 }               
    return ()

getPlaylist keywords = do
    uid <- getsST st_uid
    let param = [ ("q", keywords)
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

getSongUrl mid = do
    let param = [("type", "NO"), ("mid", mid)]
    json <- jingRequest "/media/song/surl?" param
    let parsed = decode $ C.pack json :: Maybe (Map String Value)
    let (String surl) = (fromJust parsed) ! "result"
    return surl

--postHeard :: IO String
postHeard = do
    uid <- getsST st_uid
    tid <- getsST st_tid
    let param = [("uid", uid), ("tid", tid)]
    json <- jingRequest "/music/post_heard_song?" param    
    --putStrLn json
    return json


jingRequest :: String -> [(String, String)] -> IO String
jingRequest path param = do
    h <- getsST headers >>= readMVar
    --putStrLn url
    let req = Request { rqURI = uri, rqMethod = POST, rqHeaders = h, rqBody = "" } 
    rsp <- simpleHTTP req
    getResponseBody rsp
  where
    url = "http://jing.fm/api/v1" ++ path ++ urlEncodeVars param
    uri = fromJust $ parseURI url



