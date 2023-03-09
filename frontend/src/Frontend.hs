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
             add <- button "Add"
             rem <- button "Remove"
             listLen
               -- | Fold over, every time a event triggeres
                <-
               foldDyn
                 (\x ->
                    case x of
                      Add n -> \acc -> Add (currentId acc + n)
                      Remove n -> \acc -> Remove (currentId acc - n))
                 (Add 0) $
               -- | Fires, if one of the events gets executed if multiple, chooses the left one
               leftmost [Add 1 <$ add, Remove 1 <$ rem]
             divClass "bg-red-700" $ dynText (T.pack . show <$> listLen)
             el "div" $ text "λ Welcome to Obelisk!"
             el "p" $ text $ T.pack commonStuff
             el "div" $ do
               t <- inputElement def
               text " "
               divClass "bg-red-700 font-fira" $ dynText $ _inputElement_value t
             el "div" $
               void $
               -- | basically literally makes a list with changing widgets based on the given Event
               listHoldWithKey
                 mempty -- Monoidic identity
                 -- | `updated` basically reevalutes this whenever the Dynamic listLen changes
                 (toMap <$> updated listLen)
                 (\_ _ -> counter)
    }

buildConsole :: DomBuilder t m => m ()
buildConsole = divClass "w-full h-40 border border-PURPLE" $ return ()

data AddRemove
  = Add Int
  | Remove Int
  deriving (Show)

currentId :: AddRemove -> Int
currentId x =
  case x of
    Add n -> n
    Remove n -> n

counter ::
     Control.Monad.Fix.MonadFix m
  => (DomBuilder t m, PostBuild t m, MonadHold t m) =>
       m ()
counter = do
  down <- button "<"
  up <- button ">"
  c <- foldDyn id (0 :: Int) $ leftmost [(+ 1) <$ up, (subtract 1) <$ down]
  dynText (T.pack . show <$> c)

-- Just x means, the widget should render, Nothing means it shouldn't
toMap :: AddRemove -> M.Map Int (Maybe ())
toMap x =
  case x of
    Add n -> M.singleton n (Just ())
    Remove n -> M.singleton (n + 1) Nothing
