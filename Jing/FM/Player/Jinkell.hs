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

main :: IO ()
main = do
    forkIO mpgInit
    runInputT defaultSettings login
    runInputT defaultSettings loop
  where 
    login :: InputT IO ()
    login = do
        Just email <- getInputLine "Email: "
        Just pwd <- getInputLine "Password: "
        liftIO $ createSession email pwd
        return ()
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "♫♮ "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do 
                liftIO $ play input []
                outputStrLn $ "Input was: " ++ input
                loop

play keywords [] = do
    u_id <- withST $ \st -> do 
        return $ uid st 
    getPlaylist (encodeString keywords) u_id >>= play keywords
play keywords (x:xs) = do
    surl <- getSongUrl $ mid x
    print surl
    print $ (atn x) ++ " - " ++ (n x)
    let input = "loadfile " ++ (show surl)
    withST $ \st -> do
        hin <- readMVar (writeh st)
        hout <- readMVar (readh st)
        hPutStrLn hin input
        hFlush hin
        mpg123wait hin hout
    play keywords xs
  where
    mpg123wait hin hout = do                                                    
        line <- hGetLine hout                                                   
        --print line
        case line of                                                            
             "EOF code: 1  " -> do                                   
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

