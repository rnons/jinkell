{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad.Reader
import Data.List                        (intersperse)
import Network.HTTP.Conduit             (newManager, def)
import System.Environment               (getArgs)
import System.Directory                 (createDirectoryIfMissing)
import System.IO

import Jing.FM
import Jing.FM.Player.Jpd.Core
import Jing.FM.Player.Jpd.Utils
import Jing.FM.Player.Jpd.State


main :: IO ()
main = do
    {- Make sure ~/.jinkell exists. -}
    exist <- getJinkellDir >>= createDirectoryIfMissing False
    
    {- Prompt display of current song title. -}
    hSetBuffering stdout NoBuffering
    
    {- Share a single Manager. -}
    mgr <- newManager def 
    mmgr <- newMVar mgr
    silentlyModifyST $ \st -> st { stMgr = mmgr }

    getArgs >>= dispatch

    {- Keep alive. -}
    getChar >> return ()

dispatch :: [String] -> IO ()
dispatch [] = help
dispatch ["help"] = help
dispatch ["stop"] = stop
dispatch x = listen $ concat $ intersperse " + " x

listen :: String -> IO ()
listen keywords = do 
    tok' <- readToken
    case tok' of
         Just t  -> do
             forkIO $ flip runReaderT t $ play keywords []
             return ()
         Nothing -> putStrLn "Token not found!" 

help = undefined

stop = undefined
