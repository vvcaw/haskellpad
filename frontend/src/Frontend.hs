{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Reflex.Dom.Core

import Common.Api
import Common.Route

import CodeComponent (codeComponent)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head =
        do el "title" $ text "Obelisk Minimal Example"
           elAttr
             "link"
             ("href" =: $(static "main.css") <>
              "type" =: "text/css" <> "rel" =: "stylesheet")
             blank
    , _frontend_body =
        do divClass "bg-WHITE font-fira w-full h-screen" $ do
             add <- button "Add component"
             listLen <- foldDyn id (1 :: Int) $ (+ 1) <$ add
             divClass "bg-red-700" $ dynText (T.pack . show <$> listLen)
             codeComponent
             el "div" $
               void $
               -- basically literally makes a list with changing widgets based on the given Event
               listHoldWithKey
                 mempty -- Monoidic identity
                 -- `updated` basically reevalutes this whenever the Dynamic listLen changes
                 (toMap <$> updated listLen)
                 -- arguments are key and value of the widget
                 (\_ _ -> codeComponent)
    }

buildConsole :: DomBuilder t m => m ()
buildConsole = divClass "w-full h-40 border border-PURPLE" $ return ()

-- Just x means, the widget should render, Nothing means it shouldn't
toMap :: Int -> M.Map Int (Maybe ())
toMap x = M.singleton x (Just ())
