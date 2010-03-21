import Graphics.GD.State

main = do
    (savePngFile "circle.png" =<<) . newImage (400,300) $ do
        (w,h) <- getSize
        drawArc
            (w `div` 2,h `div` 2) -- centered
            (180,180) -- (width,height)
            0 360 -- a circle
            (rgb 255 255 255) -- white circle
        fill $ rgb 100 63 127 -- dark purple background
