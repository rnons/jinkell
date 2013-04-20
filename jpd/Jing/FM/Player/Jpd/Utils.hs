{-# LANGUAGE OverloadedStrings #-}
module Jing.FM.Player.Jpd.Utils where

import Control.Applicative
import qualified Data.Configurator as CF
import System.Directory (getHomeDirectory, doesFileExist)

import Jing.FM (Token(..))

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

getJinkellDir :: IO FilePath
getJinkellDir = do
    home <- getHomeDirectory
    return $ home ++ "/.jinkell"
