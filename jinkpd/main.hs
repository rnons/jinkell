import Control.Concurrent
import Control.Concurrent.Lifted hiding (newMVar)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Reader
import Network.HTTP.Conduit             (newManager, def)
import System.Console.Haskeline
import System.Directory                 (createDirectoryIfMissing)
import System.IO

import Jing.FM
import Jing.FM.Player.Jinkpd
import Jing.FM.Player.Jinkpd.State


main :: IO ()
main = do
    {- Make sure ~/.jinkell exists. -}
    exist <- getJinkellDir >>= createDirectoryIfMissing False
    
    {- Prompt display of current song title. -}
    hSetBuffering stdout NoBuffering
    
    {- TODO: check token is valid. -}
    tok' <- readToken
    tok <- case tok' of
                Just t  -> return t
                Nothing -> runInputT defaultSettings login
    print $ "Welcome back, " ++ jingNick tok 
    putStrLn "用大白话描述出你想听的音乐"
    
    {- Share a single Manager. -}
    mgr <- newManager $ def 
    mmgr <- newMVar mgr
    silentlyModifyST $ \st -> st { stMgr = mmgr }
    
    {- Main loop. -}
    runInputT defaultSettings $ loop tok
  where
    login :: InputT IO Token
    login = do
        Just email <- getInputLine "Email: "
        Just pwd <- getPassword Nothing "Password: "
        mtoken <- liftIO $ createSession email pwd
        case mtoken of
             Just tok -> do
                 return tok
             Nothing  -> do
                 liftIO $ putStrLn "Invalid email or password!"
                 login

loop :: Token -> InputT IO ()
loop tok = do
    minput <- getInputLine "♫♮ "
    case minput of
        Nothing -> liftIO shutdown
        Just ":quit" -> liftIO shutdown
        Just "" -> loop tok
        Just input -> do
            lift $ flip runReaderT tok $ do
                case input of
                     ":p"     -> lift pause
                     ":n"     -> lift next
                     ":pause" -> lift pause
                     ":next"  -> lift next
                     ":love"  -> love
                     ":hate"  -> hate
                     ":save"  -> saveToken
                     ":help"  -> lift help
                     _        -> do
                         lift $ silentlyModifyST $ \st -> st { st_cmbt = input }
                         fork $ play input []
                         return ()
            loop tok