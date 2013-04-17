{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jinkpd where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Exception        (catch)
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import qualified Data.Configurator as CF
import Network.HTTP.Conduit
import qualified Network.MPD as MPD
import System.Directory (doesFileExist)

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
    tok <- ask
    --lift $ print surl
    lift $ do
        putStrLn $ atn x ++ " - " ++ n x
        putStr "♫♮ "
        req <- parseUrl surl
        home <- getJinkellDir
        manager <- getsST stMgr >>= readMVar
        threadId <- forkIO $ catch 
            (do
                --simpleHttp surl >>= L.writeFile (home ++ "/jinkell.m4a")
                runResourceT $ do 
                    response <- http req manager
                    -- File streamed to ~/.jinkell/jinkell.m4a.
                    -- Don't Forget to sym link ~/.jinkell to music dir of mpd!
                    responseBody response $$+- sinkFile (home ++ "/jinkell.m4a")
                d <- getsST downloaded
                putMVar d ())
            (\e -> do
                print (e :: HttpException)
                runReaderT (play keywords xs) tok)
                --next)
        mtid <- newMVar threadId
        silentlyModifyST $ \st -> st { st_tid = show $ tid x
                                     , st_atn = atn x
                                     , st_n   = n x
                                     , status = True
                                     , stThreadId = mtid
                                     }
    lift $ do
        mpdLoad
        return ()
    play keywords xs

mpdLoad = do
    threadDelay 1000000
    s <- MPD.withMPD $ do
            MPD.clear
            MPD.update []
            MPD.add "jinkell"
    case s of
        Right [MPD.Path _] -> do
            MPD.withMPD $ MPD.play Nothing
            play'
        _                  -> mpdLoad

play' = do
    s <- MPD.withMPD $ MPD.idle [MPD.PlayerS]   -- block until paused/finished
    st <- MPD.withMPD MPD.status
    let st' = fmap MPD.stState st
    --print st'
    d <- getsST downloaded 
    bd <- isEmptyMVar d
    if st' == Right MPD.Stopped 
        then if bd 
                then do                                     -- Slow Network
                    MPD.withMPD $ MPD.play Nothing
                    play'
                else takeMVar d                             -- Finished
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
    home <- getJinkellDir
    let path = home ++ "/jinkell.cfg"
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
