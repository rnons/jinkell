{-# LANGUAGE OverloadedStrings #-}

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
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
        case minput of
            Nothing -> return ()
            Just "pause" -> liftIO $ pause
            Just "quit" -> do
                liftIO $ send "quit"
                return ()
            Just input -> do
                liftIO $ forkIO $ play input []
                outputStrLn $ "Input was: " ++ input
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
                                 }
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hout <- readMVar (readh st)
        mpg123wait hin hout
    play keywords xs
  where
    mpg123wait hin hout = do
        line <- hGetLine hout
        case line of
             "EOF code: 1  " -> do
                 postHeard
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

