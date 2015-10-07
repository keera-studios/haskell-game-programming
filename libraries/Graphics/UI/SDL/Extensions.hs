module Graphics.UI.SDL.Extensions where

import Graphics.UI.SDL as SDL

-- ** Auxiliary SDL stuff

-- Auxiliary SDL stuff
isEmptyEvent :: SDL.Event -> Bool
isEmptyEvent SDL.NoEvent = True
isEmptyEvent _           = False
