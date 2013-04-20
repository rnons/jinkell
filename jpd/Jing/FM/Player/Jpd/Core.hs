{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jpd.Core where

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
        threadDelay 3000000
        mpdLoad
        return ()
    play keywords xs

mpdLoad = do
    s <- MPD.withMPD $ do
            MPD.clear
            MPD.update []
            MPD.add "jinkell"
    case s of
        Right [MPD.Path _] -> do
            MPD.withMPD $ MPD.play Nothing
            mpdPlay
        _                  -> mpdLoad

mpdPlay = do
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
                    mpdPlay
                else takeMVar d                             -- Finished
        else mpdPlay                                        -- Pause

