{-
    Graphics.GD.State
    License: BSD3
    Author: James Halliday
-}

module Graphics.GD.State (
    GD,channels,
    withImage,withNewImage,newImage,
    getSize,getImage,eachPixel,setPixels,
    setPixel,copyRegion,copyRegionScaled,resize,rotate,fill,
    drawFilledRectangle,drawFilledEllipse,drawLine,drawArc,
    drawString,drawStringCircle,
    module Graphics.GD
) where

import qualified Graphics.GD as GD
import Graphics.GD (
        Point,Color,Image,Size,
        copyImage,rgb,rgba,
        loadJpegFile,loadJpegData,loadJpegByteString,
        loadPngFile,loadPngData,loadPngByteString,
        loadGifFile,loadGifData,loadGifByteString,
        saveJpegFile,saveJpegByteString,
        savePngFile,savePngByteString,
        saveGifFile,saveGifByteString
    )

import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Control.Monad (mapM_,foldM_)
import System.IO.Unsafe (unsafePerformIO)

data GDCmd
    = CopyRegion Point Size Image Point
    | CopyRegionScaled Point Size Image Point Size
    | Resize Int Int
    | Rotate Int
    | Fill Color
    | DrawFilledRectangle Point Point Color
    | DrawFilledEllipse Point Size Color
    | DrawLine Point Point Color
    | DrawArc Point Size Int Int Color
    | SetPixel Point Color
    | DrawString String Double Double Point String Color
    | DrawStringCircle Point Double Double Double String Double String String Color

data GD'
    = GD' {
        gdCmds :: [GDCmd],
        gdImage :: Image,
        gdSize :: Size
    }

type GD a = State GD' a

channels :: Color -> (Int,Int,Int)
channels c = (r,g,b) where
    b = c' `mod` 256
    g = (c' `div` 256) `mod` 256
    r = (c' `div` 256 `div` 256) `mod` 256
    c' = fromIntegral c

newGD :: Image -> IO GD'
newGD im = do
    (w,h) <- GD.imageSize im
    return $ GD' {
        gdCmds = [],
        gdImage = im,
        gdSize = (w,h)
    }

withImage :: Image -> GD a -> IO a
withImage im f = do
    gd <- newGD =<< GD.copyImage im
    let
        (value,gd') = runState f gd
        g :: Image -> GDCmd -> IO Image
        g im cmd = g' <$> runCmd cmd im where
            g' mIm = case mIm of
                Just im -> im
                Nothing -> im
    foldM_ g (gdImage gd') $ reverse $ gdCmds gd'
    return value

withNewImage :: Size -> GD a -> IO a
withNewImage size f = ($ f) <$> withImage =<< GD.newImage size

newImage :: Size -> GD () -> IO Image
newImage size f = withNewImage size (f >> getImage)

consCmd :: GDCmd -> GD ()
consCmd cmd = modify $ \gd -> gd { gdCmds = cmd : (gdCmds gd) }

runCmd :: GDCmd -> Image -> IO (Maybe Image)

runCmd (SetPixel pt c) = (>> return Nothing) . GD.setPixel pt c

runCmd (CopyRegion pt1 size im pt2) = (>> return Nothing)
    . GD.copyRegion pt1 size im pt2

runCmd (CopyRegionScaled pt1 size1 im pt2 size2) = (>> return Nothing)
    . GD.copyRegionScaled pt1 size1 im pt2 size2

runCmd (Resize i j) = (Just <$>) . GD.resizeImage i j

runCmd (Rotate i) = (Just <$>) . GD.rotateImage i

runCmd (Fill c) = (>> return Nothing) . GD.fillImage c

runCmd (DrawFilledRectangle pt1 pt2 c) = (>> return Nothing)
    . GD.drawFilledRectangle pt1 pt2 c

runCmd (DrawFilledEllipse pt size c) = (>> return Nothing)
    . GD.drawFilledEllipse pt size c

runCmd (DrawLine pt1 pt2 c) = (>> return Nothing)
    . GD.drawLine pt1 pt2 c

runCmd (DrawArc pt size i j c) = (>> return Nothing)
    . GD.drawArc pt size i j c

runCmd (DrawString s1 x y pt s2 c) = (>> return Nothing)
    . GD.drawString s1 x y pt s2 c

runCmd (DrawStringCircle pt x y z s1 w s2 s3 c) = (>> return Nothing)
    . GD.drawStringCircle pt x y z s1 w s2 s3 c

eachPixel :: (Point -> Color) -> GD ()
eachPixel f = do
    (w,h) <- getSize
    mapM_ (\pt -> setPixel pt $ f pt)
        $ liftM2 (,) [0..w-1] [0..h-1]

setPixels :: [Color] -> GD ()
setPixels pix = do
    (w,h) <- getSize
    mapM_ (\(c,pt) -> setPixel pt c)
        $ zip pix
        $ liftM2 (,) [0..w-1] [0..h-1]

setPixel :: Point -> Color -> GD ()
setPixel = (consCmd .) . SetPixel

copyRegion :: Point -> Size -> Image -> Point -> GD ()
copyRegion = (((consCmd .) .) .) . CopyRegion

copyRegionScaled :: Point -> Size -> Image -> Point -> Size -> GD ()
copyRegionScaled = ((((consCmd .) .) .) .) . CopyRegionScaled

resize :: Int -> Int -> GD ()
resize = (consCmd .) . Resize

rotate :: Int -> GD ()
rotate = consCmd . Rotate

fill :: Color -> GD ()
fill = consCmd . Fill

drawFilledRectangle :: Point -> Point -> Color -> GD ()
drawFilledRectangle = ((consCmd .) .) . DrawFilledRectangle

drawFilledEllipse :: Point -> Size -> Color -> GD ()
drawFilledEllipse = ((consCmd .) .) . DrawFilledEllipse

drawLine :: Point -> Point -> Color -> GD ()
drawLine = ((consCmd .) .) . DrawLine

drawArc :: Point -> Size -> Int -> Int -> Color -> GD ()
drawArc = ((((consCmd .) .) .) .) . DrawArc

drawString :: String -> Double -> Double -> Point -> String -> Color -> GD ()
drawString = (((((consCmd .) . ) .) .) .) . DrawString

drawStringCircle :: Point -> Double -> Double -> Double -> String -> Double
    -> String -> String -> Color -> GD ()
drawStringCircle = ((((((((consCmd .) .) .) .) .) .) .) .) . DrawStringCircle

getImage :: GD Image
getImage = gdImage <$> get

getSize :: GD Size
getSize = gdSize <$> get

measureString :: String -> Double -> Double -> Point -> String -> Color
    -> (Point, Point, Point, Point)
measureString = (((((unsafePerformIO .) .) .) .) .) . GD.measureString
