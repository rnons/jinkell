import Control.Concurrent
import Control.Concurrent.Lifted
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
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
    hSetBuffering stdout NoBuffering
    tok <- runInputT defaultSettings login
    forkIO $ mpWait tok
    runInputT defaultSettings $ loop tok
  where
    login :: InputT IO Token
    login = do
        Just email <- getInputLine "Email: "
        Just pwd <- getPassword Nothing "Password: "
        mtoken <- liftIO $ createSession email pwd
        case mtoken of
             Just tok -> do
                 liftIO $ print $ "Welcome back, " ++ jingNick tok 
                 liftIO $ putStrLn "用大白话描述出你想听的音乐"
                 return tok
             Nothing  -> do
                 liftIO $ putStrLn "Invalid email or password!"
                 login

loop :: Token -> InputT IO ()
loop tok = do
    minput <- getInputLine "♫♮ "
    case minput of
        Nothing -> liftIO shutdown
        Just "quit" -> liftIO shutdown
        Just "" -> loop tok
        Just input -> do
            lift $ flip runReaderT tok $ do
                case input of
                     "pause" -> lift pause
                     "next"  -> lift $ send "stop"
                     "love"  -> love
                     "hate"  -> hate
                     "help"  -> lift help
                     _       -> do
                         lift $ silentlyModifyST $ \st -> st { st_cmbt = input }
                         fork $ play input []
                         return ()
            loop tok
