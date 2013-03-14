import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import System.Console.Haskeline
import System.IO
import System.Process

import Jing.FM
import Jing.FM.Player.Jinkell
import Jing.FM.Player.Jinkell.State

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
        tok <- liftIO $ createSession email pwd
        liftIO $ do
            mtok <- newMVar (fromJust tok)
            silentlyModifyST $ \st -> st { token = mtok }
            print $ "Welcome back, " ++ (jingNick $ fromJust tok)
            putStrLn "用大白话描述出你想听的音乐"
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
