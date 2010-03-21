module Graphics.GD.Monad (
    GD, withImage, newImage,
    setPixel, getSize,
    rgb, rgba
) where

import qualified Graphics.GD as GD
import Graphics.GD (rgb,rgba)

import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Control.Monad (mapM_)

data GD'
    = GD' {
        gdCmds :: [GDCmd],
        gdImage :: GD.Image,
        gdSize :: GD.Size
    }

newGD :: GD.Image -> IO GD'
newGD im = do
    size <- GD.imageSize im
    return $ GD' [] im size

data GDCmd
    = CopyImage
    | SetPixel GD.Point GD.Color
    | ImageSize

data GDRet
    = GDZero
    | GDSize GD.Size

type GD a = State GD' a

withImage :: GD.Image -> GD a -> IO a
withImage im f = do
    gd <- newGD =<< GD.copyImage im
    let im' = gdImage gd
        (value,gd') = runState f gd
    mapM_ (flip runCmd $ im') $ gdCmds gd'
    return value

newImage :: GD.Size -> GD () -> IO GD.Image
newImage size f = ($ f') <$> withImage =<< GD.newImage size
    where f' = f >> getImage

consCmd :: GDCmd -> GD ()
consCmd cmd = modify $ \gd -> gd { gdCmds = cmd : (gdCmds gd) }

runCmd :: GDCmd -> GD.Image -> IO ()
runCmd (SetPixel point color) = GD.setPixel point color

--mapGD :: (GD.Point -> GD.Color) -> GD ()
--mapGD f = mapM_ (\pt -> setPixel pt $ f pt) $ 

setPixel :: GD.Point -> GD.Color -> GD ()
setPixel point color = consCmd $ SetPixel point color

getImage :: GD GD.Image
getImage = gdImage <$> get

getSize :: GD GD.Size
getSize = gdSize <$> get
