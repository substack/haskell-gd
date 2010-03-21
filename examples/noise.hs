import Graphics.GD.State
import System.Random

main = do
    g <- newStdGen
    (savePngFile "noise.png" =<<) . newImage (400,300) $ do
        setPixels $ map fromInteger $ randomRs (0,256^3-1) g
