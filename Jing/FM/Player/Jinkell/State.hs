module Jing.FM.Player.Jinkell.State where

import System.IO.Unsafe         (unsafePerformIO)
import System.IO                (Handle)

import Control.Concurrent.MVar
import Network.HTTP.Headers     (Header)

data JState = JState 
    { writeh          ::  MVar Handle
    , readh           ::  MVar Handle
    , status          ::  Bool
    , headers         ::  MVar [Header]
    , st_uid          ::  String
    , st_nick         ::  String
    , st_atn          ::  String
    , st_n            ::  String
    , st_tid          ::  String
    , st_s_picture    ::  String
    , st_s_albumtitle ::  String
    , st_s_album      ::  String
    , st_s_like       ::  Int
    , st_s_artist     ::  String
    , st_s_title      ::  String
    , st_s_sid        ::  String
    , st_s_ssid       ::  String
    , st_s_aid        ::  String
    }

--
-- | The initial state
--
emptySt :: JState
emptySt = JState 
    { writeh          =   unsafePerformIO newEmptyMVar
    , readh           =   unsafePerformIO newEmptyMVar
    , status          =   True
    , headers         =   unsafePerformIO newEmptyMVar
    , st_uid          =   ""
    , st_nick         =   ""
    , st_atn          =   ""
    , st_n            =   ""
    , st_tid          =   ""
    , st_s_picture    =   ""
    , st_s_albumtitle =   ""
    , st_s_album      =   ""
    , st_s_like       =   0
    , st_s_artist     =   ""
    , st_s_title      =   ""
    , st_s_sid        =   ""
    , st_s_ssid       =   ""
    , st_s_aid        =   ""    
    }

--
-- | A global variable holding the state. Todo StateT
--
state :: MVar JState
state = unsafePerformIO $ newMVar emptySt

------------------------------------------------------------------------
-- state accessor functions

-- | Access a component of the state with a projection function
getsST :: (JState -> a) -> IO a
getsST f = withST (return . f)

-- | Perform a (read-only) IO action on the state
withST :: (JState -> IO a) -> IO a
withST f = readMVar state >>= f

-- | Modify the state with a pure function
silentlyModifyST :: (JState -> JState) -> IO ()
silentlyModifyST  f = modifyMVar_ state (return . f)

