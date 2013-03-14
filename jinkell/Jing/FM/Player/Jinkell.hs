module Jing.FM.Player.Jinkell where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import System.IO
import System.Process

import Jing.FM
import Jing.FM.Player.Jinkell.State


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

-- | If song endded or skipped, putMVar.
mpWait = do
    hout <- getsST readh >>= readMVar
    line <- hGetLine hout
    case line of
         "EOF code: 1  " -> do      -- song end
             ended <- getsST st_ended
             tid <- getsST st_tid
             tok <- getsST token >>= readMVar
             postHeard tok tid
             putMVar ended ()
         "EOF code: 4  " -> do      -- next song
             ended <- getsST st_ended
             putMVar ended ()
         _ -> return ()
    mpWait

-- | If playlist is empty, fire `getPlaylist`.
--   Otherwise, just play sequently.
play :: String -> [PlsItem] -> IO ()
play keywords [] = do
    tok <- getsST token >>= readMVar
    pls <- getPlaylist tok keywords
    case pls of
        [] -> putStrLn "Nothing here" >> putStr "♫♮ "
        _  -> play keywords pls
play keywords (x:xs) = do
    tok <- getsST token >>= readMVar
    surl <- getSongUrl tok $ mid x
    --print surl
    putStrLn $ (atn x) ++ " - " ++ (n x)
    putStr "♫♮ "
    let input = "loadfile " ++ surl
    send input
    silentlyModifyST $ \st -> st { st_tid = show $ tid x
                                 , st_atn = atn x
                                 , st_n   = n x
                                 , status = True
                                 }
    ended <- getsST st_ended
    takeMVar ended      -- If song playing, block.
    play keywords xs

pause :: IO ()
pause = do
    send "pause"
    status <- getsST status
    if status then do
                silentlyModifyST $ \st -> st { status = False }
                putStrLn "Paused"
              else do
                silentlyModifyST $ \st -> st { status = True }
                putStrLn "Playing"

-- | Love current playing song.
love :: IO ()
love = do
    uid  <- getsST st_uid
    tid  <- getsST st_tid
    cmbt <- getsST st_cmbt
    tok <- getsST token >>= readMVar
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                , ("moodTagIds", "")
                ]
    postLove tok param
    return ()

-- | Hate current playing song.
hate :: IO ()
hate = do
    uid  <- getsST st_uid
    tid  <- getsST st_tid
    cmbt <- getsST st_cmbt
    tok <- getsST token >>= readMVar
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                ]
    postHate tok param
    send "stop"     -- next song

-- | All messages to `mplayer` is sent from here.
send :: String -> IO ()
send msg = withST $ \st -> do
    hin <- readMVar (writeh st)
    hPutStrLn hin msg
    hFlush hin

-- | Take the handle of `mplayer`, then terminate.
shutdown :: IO ()
shutdown = do
    hdl <- getsST mpHdl >>= readMVar
    terminateProcess hdl



