module DOM where

import Custom.Prelude
import Data.Array (head)
import Data.Traversable (for)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document (createElement, toNonElementParentNode)
import Web.DOM.Element as Element
import Web.DOM.MutationObserver (MutationObserver, mutationObserver, observe)
import Web.DOM.MutationRecord (MutationRecord, target)
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (head, toDocument) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

-- import Data.UUID as UUID
installMutationObserver :: Node.Node -> Effect Unit
installMutationObserver node = do
  observer <- mutationObserver performOnChange
  _ <- observe node { childList: true } observer
  pure unit

performOnChange :: Array MutationRecord -> MutationObserver -> Effect Unit
performOnChange records obs = do
  -- Equivalent of:
  -- recs[0].target.classList.toggle('overflowing', this.scrollWidth > this.clientWidth);
  case head records of
    Nothing -> pure unit
    Just record -> do
      node <- target record
      case Element.fromNode node of
        Nothing -> pure unit
        Just element -> do
          list <- Element.classList element
          scrollWidth <- Element.scrollWidth element
          clientWidth <- Element.clientWidth element
          _ <- DOMTokenList.toggleForce list "overflowing" (scrollWidth > clientWidth)
          pure unit
      pure unit

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
