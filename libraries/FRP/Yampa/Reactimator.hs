module FRP.Yampa.Reactimator where

import FRP.Yampa as Yampa

-- * Yampa Extensions

-- ** Reactimate forever, obtaining new fresh inputs every time
alwaysReactimate :: IO a -> (IO (DTime, a)) -> (b -> IO ()) -> SF a b -> IO ()
alwaysReactimate initialSense sense consume sf =
  reactimate initialSense
             (\_ -> do
               (dt, sp) <- sense
               return (dt, Just sp)
             )
             (\_ e -> consume e >> return False)
             sf

-- -- ** High-level reactimation, from a signal generator to a sink
--
-- {-# LANGUAGE ScopedTypeVariables #-}
--
-- type YampaSystem a b c d = SF c d
--
-- -- I can define the following, but not use it the way I would expect
-- hlReactimate :: forall a b c d . (Signal a c IO, Sink b d IO)
--              => YampaSystem a b c d
--              -> IO ()
-- hlReactimate sf = do
--   (producer, initSample) <- initializeSg :: IO (a, c)
--   consumer               <- initializeSi :: IO b
--   alwaysReactimate (return initSample)
--                    (pollSg producer)
--                    (pushSi consumer)
--                    sf
