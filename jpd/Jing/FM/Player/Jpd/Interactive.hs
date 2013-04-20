{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jpd.Interactive where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Exception        (catch)
import Control.Monad
import Control.Monad.Reader
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Network.MPD as MPD

import Jing.FM
import Jing.FM.Player.Jpd.State
import Jing.FM.Player.Jpd.Utils

pause :: IO ()
pause = do
    status <- getsST status
    MPD.withMPD $ MPD.pause status
    if status then do
                silentlyModifyST $ \st -> st { status = False }
                putStrLn "Paused"
              else do
                silentlyModifyST $ \st -> st { status = True }
                putStrLn "Playing"

-- | Love current playing song.
love :: ReaderT Token IO ()
love = do
    uid  <- liftIO $ getsST st_uid
    tid  <- liftIO $ getsST st_tid
    cmbt <- liftIO $ getsST st_cmbt

    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                , ("moodTagIds", "")
                ]
    postLove param
    return ()

-- | Hate current playing song.
hate :: ReaderT Token IO ()
hate = do
    uid  <- liftIO $ getsST st_uid
    tid  <- liftIO $ getsST st_tid
    cmbt <- liftIO $ getsST st_cmbt
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                ]
    postHate param
    liftIO next     -- next song


saveToken :: ReaderT Token IO ()
saveToken = do
    tok <- ask
    liftIO $ do
        home <- getJinkellDir
        writeFile (home ++ "/jinkell.cfg") $ pprToken tok

pprToken tok = unlines [ "token"
                       , "{"
                       , "    atoken = \"" ++ jingAToken tok ++ "\""
                       , "    rtoken = \"" ++ jingRToken tok ++ "\""
                       , "    uid    = \"" ++ jingUid tok ++ "\""
                       , "    nick   = \"" ++ jingNick tok ++ "\""
                       , "}" ]

help :: IO ()
help = putStrLn $ unlines msg
  where
    msg = [ "Commands:"
          , ":pause                  pause/play"
          , ":next                   next song"
          , ":love                   love current song"
          , ":hate                   hate current song"
          , ":save                   save email/password in $HOME/.jinkell.cfg"
          , ":help                   this message"
          ]

next = do
    tid <- getsST stThreadId >>= readMVar
    killThread tid                  -- Release response and file handler
    d <- getsST downloaded 
    bd <- isEmptyMVar d
    when bd $ putMVar d ()          -- Prevent play' from blocking
    void $ MPD.withMPD MPD.stop

shutdown :: IO ()
shutdown = do
    MPD.withMPD MPD.clear
    return ()
