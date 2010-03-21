import Graphics.GD.Monad
import Control.Monad (forM_,liftM2)
import Control.Monad.Trans (liftIO)

main = do
    (print =<<) . withNewImage (400,300) $ do
        forM_ (liftM2 (,) [1..400] [1..300]) $ \pt -> do
            setPixel pt $ rgb 230 40 20
        getSize
