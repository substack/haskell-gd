module Graphics.GD.State (
    GD,
    withImage, withNewImage, newImage,
    saveJpegFile,saveJpegByteString,
    savePngFile,savePngByteString,
    saveGifFile,saveGifByteString,
    getSize, getImage,
    setPixel, eachPixel, setPixels,
    rgb, rgba, channels
) where

import qualified Graphics.GD as GD
import Graphics.GD (
        rgb,rgba,
        saveJpegFile,saveJpegByteString,
        savePngFile,savePngByteString,
        saveGifFile,saveGifByteString
    )

import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Control.Monad (mapM_)
import Control.Monad.Trans (lift,liftIO)

data GDCmd
    = CopyImage
    | SetPixel GD.Point GD.Color
    | ImageSize

data GDRet
    = GDZero
    | GDSize GD.Size

data GD'
    = GD' {
        gdCmds :: [GDCmd],
        gdImage :: GD.Image,
        gdSize :: GD.Size
    }

type GD a = State GD' a

channels :: GD.Color -> (Int,Int,Int)
channels c = (r,g,b) where
    b = c' `mod` 256
    g = (c' `div` 256) `mod` 256
    r = (c' `div` 256 `div` 256) `mod` 256
    c' = fromIntegral c

newGD :: GD.Image -> IO GD'
newGD im = do
    (w,h) <- GD.imageSize im
    return $ GD' {
        gdCmds = [],
        gdImage = im,
        gdSize = (w,h)
    }

withImage :: GD.Image -> GD a -> IO a
withImage im f = do
    gd <- newGD =<< GD.copyImage im
    let im' = gdImage gd
        (value,gd') = runState f gd
    mapM_ (flip runCmd $ im') $ gdCmds gd'
    return value

withNewImage :: GD.Size -> GD a -> IO a
withNewImage size f = ($ f) <$> withImage =<< GD.newImage size

newImage :: GD.Size -> GD () -> IO GD.Image
newImage size f = withNewImage size (f >> getImage)

consCmd :: GDCmd -> GD ()
consCmd cmd = modify $ \gd -> gd { gdCmds = cmd : (gdCmds gd) }

runCmd :: GDCmd -> GD.Image -> IO ()
runCmd (SetPixel point color) = GD.setPixel point color

eachPixel :: (GD.Point -> GD.Color) -> GD ()
eachPixel f = do
    (w,h) <- getSize
    mapM_ (\pt -> setPixel pt $ f pt)
        $ liftM2 (,) [0..w-1] [0..h-1]

setPixels :: [GD.Color] -> GD ()
setPixels pix = do
    (w,h) <- getSize
    mapM_ (\(c,pt) -> setPixel pt c)
        $ zip pix
        $ liftM2 (,) [0..w-1] [0..h-1]

setPixel :: GD.Point -> GD.Color -> GD ()
setPixel point color = consCmd $ SetPixel point color

getImage :: GD GD.Image
getImage = gdImage <$> get

getSize :: GD GD.Size
getSize = gdSize <$> get
