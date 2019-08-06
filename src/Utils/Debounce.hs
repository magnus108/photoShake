{-# LANGUAGE ScopedTypeVariables #-}
module Utils.Debounce
    ( DebounceSettings(..)
    , defaultDebounceSettings
    , mkDebounce
    , leadingEdge
    , trailingEdge
    ) where

import Control.Concurrent.MVar (takeMVar, tryPutMVar, tryTakeMVar, MVar)
import Control.Exception (SomeException, handle, mask_)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, newEmptyMVar, threadDelay)

defaultDebounceSettings :: DebounceSettings
defaultDebounceSettings = DebounceSettings
    { debounceFreq = 1000000
    , debounceAction = return ()
    , debounceEdge = leadingEdge
    }

mkDebounce :: DebounceSettings -> IO (IO ())
mkDebounce settings = do
  baton <- newEmptyMVar
  mkDebounceInternal baton threadDelay settings


data DebounceSettings = DebounceSettings
    { debounceFreq   :: Int
    , debounceAction :: IO ()
    , debounceEdge :: DebounceEdge
    }

data DebounceEdge 
  = Leading
  | Trailing
  deriving (Show, Eq)


leadingEdge :: DebounceEdge
leadingEdge = Leading

trailingEdge :: DebounceEdge
trailingEdge = Trailing


mkDebounceInternal :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
mkDebounceInternal baton delayFn (DebounceSettings freq action edge) = do
    mask_ $ void $ forkIO $ forever $ do
        takeMVar baton
        case edge of
          Leading -> do
            ignoreExc action
            delayFn freq
          Trailing -> do
            delayFn freq
            void $ tryTakeMVar baton
            ignoreExc action

    return $ void $ tryPutMVar baton ()

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
