import Graphics.GD.State

main = (savePngFile "gradient.png" =<<) . newImage (400,300) $ do
    (w,h) <- getSize
    eachPixel $ \(x,y) ->
        let
            r = ((128 * x) `div` w) + ((128 * y) `div` h)
            g = 127 + ((128 * x) `div` w)
            b = 127
        in rgb r g b
