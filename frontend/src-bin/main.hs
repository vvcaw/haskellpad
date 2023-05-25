import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

myWidget :: (MonadWidget t m) => m ()
myWidget = do
    result <- liftIO $ runInterpreter $ do
        setImports ["Prelude"]
        interpret "1 + 1" (const True :: [Bool] -> Bool)
    return ()

main :: IO ()
main = mainWidget myWidget --do
--  let Right validFullEncoder = checkEncoder fullRouteEncoder
--  run $ runFrontend validFullEncoder frontend
