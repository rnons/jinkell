{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jinkpd where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import qualified Data.Configurator as CF
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.MPD as MPD
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO
import System.Process

import Jing.FM
import Jing.FM.Player.Jinkpd.State


-- | If playlist is empty, fire `getPlaylist`.
--   Otherwise, just play sequently.
play :: String -> [PlsItem] -> ReaderT Token IO ()
play keywords [] = do
    pls <- getPlaylist keywords
    case pls of
        [] -> lift $ putStrLn "Nothing here" >> putStr "♫♮ "
        _  -> play keywords pls
play keywords (x:xs) = do
    surl <- getSongUrl $ mid x
    --lift $ print surl
    lift $ do
        manager <- getsST stMgr >>= readMVar
        putStrLn $ (atn x) ++ " - " ++ (n x)
        putStr "♫♮ "
        req <- parseUrl surl
        hdl <- forkIO $ runResourceT $ do
            response <- http req manager
            responseBody response $$+- sinkFile "/home/rnons/Music/jinkell/jinkell.m4a"
            lift $ do
                d <- getsST downloaded
                putMVar d ()
        mhdl <- newMVar hdl
        silentlyModifyST $ \st -> st { st_tid = show $ tid x
                                     , st_atn = atn x
                                     , st_n   = n x
                                     , status = True
                                     , stHdl = mhdl
                                     }
    lift $ do
        mpd'
        return ()
    play keywords xs

mpd' = do
    threadDelay 1000000
    s <- MPD.withMPD $ do
            MPD.clear
            MPD.update []
            MPD.add "jinkell"
    case s of
        Right [MPD.Path _] -> do
            MPD.withMPD $ MPD.play Nothing
            play'
        _                  -> mpd'

play' = do
    s <- MPD.withMPD $ MPD.idle [MPD.PlayerS]   -- block until paused/finished
    st <- MPD.withMPD MPD.status
    let st' = fmap MPD.stState st
    d <- getsST downloaded 
    bd <- isEmptyMVar d
    if st' == Right MPD.Stopped 
        then if bd 
                then do                                     -- Slow Network
                    MPD.withMPD $ MPD.play Nothing
                    play'
                else do                                     -- Finished
                    takeMVar d                              
        else play'                                          -- Pause

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

readToken :: IO (Maybe Token)
readToken = do
    home <- getHomeDirectory
    let path = home ++ "/.jinkell.cfg"
    exist <- doesFileExist path
    if exist
       then do
            conf <- CF.load [ CF.Required path ]
            atoken <- CF.lookup conf "token.atoken" :: IO (Maybe String)
            rtoken <- CF.lookup conf "token.rtoken" :: IO (Maybe String)
            uid <- CF.lookup conf "token.uid" :: IO (Maybe String)
            nick <- CF.lookup conf "token.nick" :: IO (Maybe String)
            return $ Token <$> atoken <*> rtoken <*> uid <*> nick
       else return Nothing

saveToken :: ReaderT Token IO ()
saveToken = do
    tok <- ask
    liftIO $ do
        home <- getHomeDirectory
        writeFile (home ++ "/.jinkell.cfg") $ pprToken tok

pprToken tok = unlines [ "token"
                       , "{"
                       , "    atoken = \"" ++ (jingAToken tok) ++ "\""
                       , "    rtoken = \"" ++ (jingRToken tok) ++ "\""
                       , "    uid    = \"" ++ jingUid tok ++ "\""
                       , "    nick   = \"" ++ jingNick tok ++ "\""
                       , "}" ]

help :: IO ()
help = do
    putStrLn $ unlines msg
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
    tid <- getsST stHdl >>= readMVar
    killThread tid                  -- Release response and file handler
    d <- getsST downloaded 
    bd <- isEmptyMVar d
    when bd $ do
        putMVar d ()                -- Prevent play' from blocking
    MPD.withMPD MPD.stop >> return ()

shutdown :: IO ()
shutdown = do
    MPD.withMPD MPD.clear
    return ()
