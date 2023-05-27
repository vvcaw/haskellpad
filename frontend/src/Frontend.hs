{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.Fix (MonadFix)

import Language.Haskell.Interpreter

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Reflex.Dom.Core

import Common.Api
import Common.Route

domB :: DomBuilder t m => m ()
domB = do
    return ()

bodyElement :: (PerformEvent t m, MonadIO (Performable m), DomBuilder t m, PostBuild t m) => m ()
bodyElement = do
  el "h1" $ text "Execute IO Action"

  buttonClick <- button "Click me"

  -- Execute an IO action when the button is clicked
  performEvent_ $ liftIOAction <$> buttonClick

liftIOAction :: MonadIO m => a -> m ()
liftIOAction _ = liftIO $ putStrLn "Hello from liftIO!"

    --result <- liftIO $ runInterpreter $ do
     --   setImports ["Prelude"]
      --  interpret "1 + 1" (const True :: [Bool] -> Bool)

myW :: (MonadIO m, DomBuilder t m, PostBuild t m) => m ()
myW = do
    result <- liftIO $ runInterpreter $ do
        setImports ["Prelude"]
        interpret "1 + 1" (const True :: [Bool] -> Bool)
    text "asdf"

app :: (MonadFix m, DomBuilder t m, MonadIO m) => m ()
app = do
    text "asdf"


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head =
        do el "title" $ text "Haskellpad"
           elAttr
             "link"
             ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =:
              "stylesheet")
             blank
    , _frontend_body = 
        do
            bu <- button "asdf"
            prerender_ blank $ performEvent_ $ (\() -> liftIO(putStrLn "asdf")) <$> bu
            performEvent_ $ (\() -> liftIO(putStrLn "lol wtf")) <$> bu
            uuidEvent <- fmap (switch . current) . prerender (return never) $ 
                performEvent (ffor bu $ \() -> liftIO (putStrLn "asdf") )
            t <-
                inputElement $ def & inputElementConfig_elementConfig .
                elementConfig_initialAttributes .~
                mconcat ["class" =: "bg-white w-auto"]
            --divClass "bg-red-700 font-fira w-full" $ dynText $
            text "asdff"--do
            t <-
                inputElement $ def & inputElementConfig_elementConfig .
                elementConfig_initialAttributes .~
                mconcat ["class" =: "bg-white w-auto"]
            --divClass "bg-red-700 font-fira w-full" $ dynText $
            performEvent_ $ (\x -> liftIO((putStrLn (T.unpack x)))) <$> (updated $ _inputElement_value t)

            {-dynText $ _inputElement_value t >>= \change -> do 
                res <- performEvent $ (\val -> return (T.unpack val)) <$> (updated $ _inputElement_value t)
                return $ T.pack res-}

            dynText $ _inputElement_value t >>= \change -> do 
                --res <- performEvent $ (\val -> return (T.unpack val)) <$> (updated $ _inputElement_value t)
                return $ T.pack $ 'a' : (T.unpack change)
                --return $ T.pack res
            
            -- Same behaviour, but returns the event with changed word as result
            ev <- performEvent $ (\val -> return (T.unpack val)) <$> (updated $ _inputElement_value t)
            performEvent_ $ (\val -> liftIO(putStrLn ("lol wtf: " ++ val))) <$> ev
            return ()
            {-divClass "w-full h-screen bg-gray-700" $ do
              divClass
                "border rounded-md border-indigo-400 p-2 space-y-2 w-full flex flex-wrap justify-around" $ do
                divClass "text-indigo-400 w-auto" $ text "λ" $ do
                    t <-
                        inputElement $ def & inputElementConfig_elementConfig .
                        elementConfig_initialAttributes .~
                        mconcat ["class" =: "bg-white w-auto"]
                    --divClass "bg-red-700 font-fira w-full" $ dynText $
                    performEvent_ $ (\x -> liftIO((putStrLn (T.pack x)))) <$> (updated $ _inputElement_value t)
            return ()-}
    }

buildConsole :: DomBuilder t m => m ()
buildConsole = divClass "w-full h-40 border border-PURPLE" $ return ()

-- Just x means, the widget should render, Nothing means it shouldn't
toMap :: Int -> M.Map Int (Maybe ())
toMap x = M.singleton x (Just ())
