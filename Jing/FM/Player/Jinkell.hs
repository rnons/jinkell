{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.IO
import System.Process
import System.Console.Haskeline
import Jing.FM.Player.Jinkell.State
import Data.Maybe
import Jing.FM
import qualified Data.Text as T

main :: IO ()
main = do
    forkIO mpgInit
    forkIO mpWait
    hSetBuffering stdout NoBuffering
    runInputT defaultSettings login
    runInputT defaultSettings loop
  where
    login :: InputT IO ()
    login = do
        Just email <- getInputLine "Email: "
        Just pwd <- getPassword Nothing "Password: "
        liftIO $ createSession email pwd
        return ()
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "♫♮ "
        case minput of
            Nothing -> liftIO shutdown
            Just "quit" -> liftIO shutdown
            Just "" -> loop
            Just input -> do
                liftIO $ do
                    case input of
                     "pause" -> pause
                     "next" -> send "stop"
                     "love" -> love
                     "hate" -> hate
                     _ -> do
                         silentlyModifyST $ \st -> st { st_cmbt = input }
                         forkIO $ play input []
                         return ()
                loop

send msg = withST $ \st -> do
    hin <- readMVar (writeh st)
    hPutStrLn hin msg
    hFlush hin

pause = do
    send "pause"
    status <- getsST status
    if status then do
                silentlyModifyST $ \st -> st { status = False }
                putStrLn "Paused"
              else do
                silentlyModifyST $ \st -> st { status = True }
                putStrLn "Playing"

love = do
    uid  <- getsST st_uid
    tid  <- getsST st_tid
    cmbt <- getsST st_cmbt
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                , ("moodTagIds", "")
                ]
    jingRequest "/music/post_love_song?" param
    return ()

hate = do
    uid  <- getsST st_uid
    tid  <- getsST st_tid
    cmbt <- getsST st_cmbt
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                ]
    jingRequest "/music/post_hate_song?" param
    -- next song
    send "stop"

play keywords [] = do
    --getPlaylist (encodeString keywords) u_id >>= play keywords
    pls <- getPlaylist keywords
    case pls of
        [] -> putStrLn "Nothing here" >> putStr "♫♮ "
        _  -> play keywords pls
play keywords (x:xs) = do
    surl <- getSongUrl $ mid x
    --print surl
    putStrLn $ (atn x) ++ " - " ++ (n x)
    putStr "♫♮ "
    let input = "loadfile " ++ (show surl)
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hPutStrLn hin input
        hFlush hin
    silentlyModifyST $ \st -> st { st_tid = show $ tid x
                                 , st_atn = atn x
                                 , st_n   = n x
                                 , status = True
                                 }
    ended <- getsST st_ended
    takeMVar ended
    play keywords xs

mpgInit = do
    let sh = "mplayer -msglevel global=6:statusline=6 -slave -idle -really-quiet -cache 2048 -cache-min 5 -novideo"
    (Just hin, Just hout, Just herr, hdl) <-
        createProcess (shell sh){ std_in = CreatePipe
                                , std_out = CreatePipe
                                , std_err = CreatePipe }
    mhin <- newMVar hin
    mhout <- newMVar hout
    mhdl <- newMVar hdl
    silentlyModifyST $ \st -> st { writeh = mhin
                                 , readh = mhout
                                 , mpHdl = mhdl }
    waitForProcess hdl
    hClose herr

mpWait = do
    hout <- getsST readh >>= readMVar
    ended <- getsST st_ended
    line <- hGetLine hout
    case line of
         "EOF code: 1  " -> do      -- song end
             postHeard
             putMVar ended ()
         "EOF code: 4  " -> do      -- next song
             putMVar ended ()
         _ -> return ()
    mpWait

shutdown = do
    hdl <- getsST mpHdl >>= readMVar
    terminateProcess hdl



