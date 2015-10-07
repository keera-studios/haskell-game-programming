{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances     #-}
module FRP.Yampa.Backends.SDL where

import Data.IORef
import FRP.Yampa.Signal as Yampa
import Graphics.UI.SDL  as SDL

-- * Signals

-- ** SDL clock as Yampa input (source)
data SDLClock = SDLClock (IORef Int)

instance Signal SDLClock Double IO where

  initializeSg = do
    timeRef <- newIORef (0 :: Int)
    return (SDLClock timeRef, 0)

  pollSg (SDLClock timeRef) = do
    
    -- Obtain new number of ticks since initialisation
    ts <- fmap fromIntegral getTicks

    -- Calculate time delta
    pt <- readIORef timeRef
    let dt  = ts - pt
        dtY = fromIntegral dt / 100 

    -- Update number of tickts
    writeIORef timeRef ts

    -- Return time delta as floating point number
    return (dtY, dtY)

data SDLSignal a = SDLSignal
  { sdlClock :: SDLClock
  , sdlValue :: a
  }

instance Source a v IO => Signal (SDLSignal a) v IO where
  initializeSg = do
    (clk,_) <- initializeSg
    so      <- initializeSo
    c       <- pollSo so
    return (SDLSignal clk so, c)

  -- Sense new input
  pollSg (SDLSignal clock input) = do
    (dt,_) <- pollSg clock
    c'     <- pollSo input
    return (dt, c')
