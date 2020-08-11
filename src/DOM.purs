module DOM where

import Custom.Prelude
import Data.Traversable (for)
import Web.DOM.Document (createElement, toNonElementParentNode)
import Web.DOM.Element (setId, toNode) as Element
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (head, toDocument) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

installStyleInHead :: String -> String -> Effect Unit
installStyleInHead id innerHTML = do
  windowItem <- liftEffect window
  documentObject <- liftEffect $ document windowItem
  element <- liftEffect $ getElementById id (toNonElementParentNode $ HTMLDocument.toDocument documentObject)
  when (isNothing element)
    $ do
        style <- liftEffect $ createElement "style" (HTMLDocument.toDocument documentObject)
        _ <- liftEffect $ Element.setId id style
        _ <- liftEffect $ Node.setTextContent innerHTML (Element.toNode style)
        headObject <- liftEffect $ HTMLDocument.head documentObject
        _ <-
          for headObject
            $ \head -> do
                liftEffect $ Node.appendChild (Element.toNode style) (HTMLElement.toNode head)
        pure unit
