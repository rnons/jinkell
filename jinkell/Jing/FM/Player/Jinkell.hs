{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jinkell where

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Configurator as CF
import qualified Data.Text as T
import System.Directory (getHomeDirectory, doesFileExist)
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
mpWait tok = do
    hout <- getsST readh >>= readMVar
    line <- hGetLine hout
    case line of
         "EOF code: 1  " -> do      -- song end
             ended <- getsST st_ended
             tid <- getsST st_tid
             postHeard tok tid
             putMVar ended ()
         "EOF code: 4  " -> do      -- next song
             ended <- getsST st_ended
             putMVar ended ()
         _ -> return ()
    mpWait tok

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
    --print surl
    let input = "loadfile " ++ surl
    lift $ do
        putStrLn $ (atn x) ++ " - " ++ (n x)
        putStr "♫♮ "
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
love :: ReaderT Token IO ()
love = do
    uid  <- liftIO $ getsST st_uid
    tid  <- liftIO $ getsST st_tid
    cmbt <- liftIO $ getsST st_cmbt

    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                , ("moodTagIds", "")
                ]
    postLove param
    return ()

-- | Hate current playing song.
hate :: ReaderT Token IO ()
hate = do
    uid  <- liftIO $ getsST st_uid
    tid  <- liftIO $ getsST st_tid
    cmbt <- liftIO $ getsST st_cmbt
    let param = [ ("uid", uid)
                , ("tid", tid)
                , ("c", "1")
                , ("cmbt", cmbt)
                ]
    postHate param
    liftIO $ send "stop"     -- next song

getJinkellDir :: IO FilePath
getJinkellDir = do
    home <- getHomeDirectory
    return $ home ++ "/.jinkell"

readToken :: IO (Maybe Token)
readToken = do
    home <- getJinkellDir
    let path = home ++ "/jinkell.cfg"
    exist <- doesFileExist path
    if exist 
       then do
            conf <- CF.load [ CF.Required path ]
            atoken <- CF.lookup conf "token.atoken" :: IO (Maybe String)
            rtoken <- CF.lookup conf "token.rtoken" :: IO (Maybe String)
            uid <- CF.lookup conf "token.uid" :: IO (Maybe String)
            nick <- CF.lookup conf "token.nick" :: IO (Maybe String)
            return $ Token <$> atoken <*> rtoken <*> uid <*> nick
       else return Nothing

saveToken :: ReaderT Token IO ()
saveToken = do
    tok <- ask
    liftIO $ do
        home <- getJinkellDir
        writeFile (home ++ "/jinkell.cfg") $ pprToken tok

pprToken tok = unlines [ "token"
                       , "{"
                       , "    atoken = \"" ++ (jingAToken tok) ++ "\""
                       , "    rtoken = \"" ++ (jingRToken tok) ++ "\""
                       , "    uid    = \"" ++ jingUid tok ++ "\""
                       , "    nick   = \"" ++ jingNick tok ++ "\""
                       , "}" ]

help :: IO ()
help = do
    putStrLn $ unlines msg
  where
    msg = [ "Commands:"
          , ":pause                  pause/play"
          , ":next                   next song"
          , ":love                   love current song"
          , ":hate                   hate current song"
          , ":save                   save email/password in $HOME/.jinkell.cfg"
          , ":help                   this message"
          ]

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



