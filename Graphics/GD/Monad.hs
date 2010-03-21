module Graphics.GD.Monad (
    GD, withImage, newImage, setPixel
) where

import qualified Graphics.GD as GD
import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Control.Monad (mapM_)

data GD' = GD' { gdCommands :: [GDCommand], gdImage :: GD.Image }

data GDCommand
    = CopyImage
    | SetPixel GD.Point GD.Color

type GDState a = State GD' a
type GD = GDState ()

withImage :: GD.Image -> GD -> IO GD.Image
withImage im f = do
    im' <- GD.copyImage im
    mapM_ (flip runCmd $ im') $ gdCommands $ execState f (GD' [] im')
    return im'

newImage :: GD.Size -> GD -> IO GD.Image
newImage size f = ($ f) <$> withImage =<< GD.newImage size

setPixel :: GD.Point -> GD.Color -> GD
setPixel point color = consCmd $ SetPixel point color

consCmd :: GDCommand -> GD
consCmd cmd = modify $ \gd -> gd { gdCommands = cmd : (gdCommands gd) }

runCmd :: GDCommand -> GD.Image -> IO ()
runCmd (SetPixel point color) = GD.setPixel point color
