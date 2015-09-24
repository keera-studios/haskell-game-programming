import Control.Concurrent
import Control.Monad
import Foreign.ForeignPtr
import Graphics.UI.SDL                as SDL
import Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

main :: IO ()
main = do
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 1024
  SDL.Mixer.Channels.allocateChannels 16
  playFile
  return ()

playFile = void $ forkOS $ do
  wave <- SDL.Mixer.Samples.loadWAV "file.wav"
  v    <- SDL.Mixer.Channels.playChannel (-1) wave 0
  threadDelay 1000000
  touchForeignPtr wave
  return ()
