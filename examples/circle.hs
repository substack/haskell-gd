import Graphics.GD.State

main = do
    (savePngFile "circle.png" =<<) . newImage (400,300) $ do
        (w,h) <- getSize
        
        fill $ rgb 100 63 127 -- dark purple background
        
        drawArc
            (w `div` 2,h `div` 2) -- centered
            (180,180) -- (width,height)
            0 360 -- a circle
            (rgb 255 255 255) -- white circle
        
        drawLine (0,0) (w-1,h-1) (rgb 127 255 127)
