module Graphics.GD.Monad (
    withImage, setPixel
) where

import qualified Graphics.GD as GD
import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Control.Monad (mapM_)

data GD = GD { gdCommands :: [GDCommand], gdImage :: GD.Image }

data GDCommand
    = CopyImage
    | SetPixel GD.Point GD.Color

type GDState a = State GD a
type GDUpdate = GDState ()

withImage :: GD.Image -> GDUpdate -> IO GD.Image
withImage im f = do
    im' <- GD.copyImage im
    mapM_ (flip runCmd $ im') $ gdCommands $ execState f (GD [] im')
    return im'

newImage :: GD.Size -> GDUpdate -> IO GD.Image
newImage size f = ($ f) <$> withImage =<< GD.newImage size

setPixel :: GD.Point -> GD.Color -> GDUpdate
setPixel point color = consCmd $ SetPixel point color

consCmd :: GDCommand -> GDUpdate
consCmd cmd = modify $ \gd -> gd { gdCommands = cmd : (gdCommands gd) }

runCmd :: GDCommand -> GD.Image -> IO ()
runCmd (SetPixel point color) = GD.setPixel point color
