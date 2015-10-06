{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Physics.TwoDimensions.PhysicalObjects where

import Physics.TwoDimensions.Dimensions

class Eq b => PhysicalObject a b c | a -> b, a -> c where
  physObjectPos       :: a -> Pos2D
  physObjectVel       :: a -> Vel2D
  physObjectElas      :: a -> Double
  physObjectShape     :: a -> c
  physObjectCollides  :: a -> Bool
  physObjectId        :: a -> b
  physObjectUpdatePos :: a -> Pos2D -> a
  physObjectUpdateVel :: a -> Vel2D -> a
