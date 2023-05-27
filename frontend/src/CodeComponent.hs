{-# LANGUAGE OverloadedStrings #-}

module CodeComponent where

import Reflex.Dom.Core

codeComponent :: (PostBuild t m, DomBuilder t m) => m ()
codeComponent = do
  divClass "" $ do
    t <-
      inputElement $ def & inputElementConfig_elementConfig .
      elementConfig_initialAttributes .~
      ("class" =: "bg-red-700")
    text " "
    divClass "bg-red-700 font-fira" $ dynText $ _inputElement_value t
