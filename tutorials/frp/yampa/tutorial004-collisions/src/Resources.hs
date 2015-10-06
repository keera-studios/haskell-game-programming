module Resources where

import qualified Graphics.UI.SDL.TTF       as TTF

-- import Game.Audio

data Resources = Resources
  { resFont  :: TTF.Font
  , miniFont :: TTF.Font
  }
