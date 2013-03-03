{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracketOnError)
import System.Exit
import System.IO
import System.Process
import System.Console.Haskeline
import Jing.FM.Player.Jinkell.State
import Data.Maybe
import Jing.FM
import qualified Data.Text as T
import System.Posix.Process (exitImmediately)

main :: IO ()
main = do
    forkIO mpgInit
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
        --outputStrLn $ fromJust minput
        case minput of
            Nothing -> do
                liftIO $ exitImmediately ExitSuccess
                return ()
            Just "quit" -> liftIO $ do
                exitImmediately ExitSuccess
                return ()
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
                --outputStrLn $ "Input was: " ++ input
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
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hout <- readMVar (readh st)
        mpg123wait hin hout
    play keywords xs
  where
    mpg123wait hin hout = do
        --print line
        line <- hGetLine hout
        case line of
             "EOF code: 1  " -> do      -- song end
                 postHeard
                 return ExitSuccess
             "EOF code: 4  " -> do      -- next song
                 return ExitSuccess
             _ -> mpg123wait hin hout

mpgInit = do
    let sh = "mplayer -msglevel global=6:statusline=6 -slave -idle -really-quiet -cache 2048 -cache-min 5 -novideo"
    (Just hin, Just hout, Just herr, hdl) <-
        createProcess (shell sh){ std_in = CreatePipe
                                , std_out = CreatePipe
                                , std_err = CreatePipe }
    mhin <- newMVar hin
    mhout <- newMVar hout
    silentlyModifyST $ \st -> st { writeh = mhin, readh = mhout }
    waitForProcess hdl
    --hPutStrLn hin "stop"
    hGetContents hout
    hGetContents herr
    hFlush hin
    return ()

