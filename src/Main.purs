module Main where

import Custom.Prelude
import Option
import Concur.React.Props (_id, _data)
import Custom.Concur as C
import DOM (installStyleInHead)

main :: Effect Unit
main = C.runWidgetInDom "root" hello

hello :: forall a. C.Widget C.HTML a
hello = do
  _ <-
    stack_ (fromRecord { space: "var(--s4)", splitAfter: Just 1 })
      [ reel_ (fromRecord { itemWidth: "50rem" })
          [ box1 $ C.h1' [ C.text "A header!" ]
          , box1 $ C.h1' [ C.text "A header!" ]
          , box1 $ C.h1' [ C.text "A header!" ]
          ]
      , C.h2' [ C.text "Second header coming up!" ]
      , C.h3' [ C.text "Lorem ipsum" ]
      ]
  C.text "Hello!"

------------------------------------------------------------------------------
-- | Template section
type ReelConfig'
  = ( itemWidth :: String
    , space :: String
    , height :: String
    , noBar :: Boolean
    )

type ReelConfig
  = Record ReelConfig'

defaultReelConfig :: ReelConfig
defaultReelConfig =
  { itemWidth: "auto"
  , space: "var(--s0)"
  , height: "auto"
  , noBar: false
  }

reel :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
reel children = reel_ (fromRecord {}) children

reel_ :: forall a. Option ReelConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
reel_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "reel-l")
    [ _data { "i": id }
    ]
    children
  where
  itemWidth = fromMaybe defaultReelConfig.itemWidth (get (SProxy :: _ "itemWidth") overrides)

  space = fromMaybe defaultReelConfig.space (get (SProxy :: _ "space") overrides)

  height = fromMaybe defaultReelConfig.height (get (SProxy :: _ "height") overrides)

  noBar = fromMaybe defaultReelConfig.noBar (get (SProxy :: _ "noBar") overrides)

  id = "Reel-" <> itemWidth <> space <> height <> show noBar

  overflowingLine =
    if noBar then
      dataId id <> ".overflowing { }"
    else
      dataId id <> ".overflowing { padding-bottom: " <> space <> "; }"

  noBarLines =
    if noBar then
      dataId id <> "{ scrollbar-width: none; } " <> dataId id <> "::-webkit-scrollbar { display: none; }"
    else
      ""

  innerHTML =
    [ dataId id <> "{ height: " <> height <> "; }"
    , dataId id <> " > * { flex: 0 0 " <> itemWidth <> "; }"
    , dataId id <> " > img { height: 100%; flex-basis: auto; width: auto; }"
    , dataId id <> " > * + * { margin-left: " <> space <> "; }"
    , overflowingLine
    , noBarLines
    ]

------------------------------------------------------------------------------
-- | Box section
type BoxConfig'
  = ( padding :: String
    , borderWidth :: String
    , invert :: Boolean
    )

type BoxConfig
  = Record BoxConfig'

defaultBoxConfig :: BoxConfig
defaultBoxConfig =
  { padding: "var(--s1)"
  , borderWidth: "var(--border-thin)"
  , invert: false
  }

box :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
box children = box_ (fromRecord {}) children

box1 :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
box1 child = box_ (fromRecord {}) [ child ]

box_ :: forall a. Option BoxConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
box_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "box-l")
    [ _data { "i": id }
    ]
    children
  where
  padding = fromMaybe defaultBoxConfig.padding (get (SProxy :: _ "padding") overrides)

  borderWidth = fromMaybe defaultBoxConfig.borderWidth (get (SProxy :: _ "borderWidth") overrides)

  invert = fromMaybe defaultBoxConfig.invert (get (SProxy :: _ "invert") overrides)

  id = "Box-" <> padding <> borderWidth <> show invert

  invertLine = if invert then "background-color: var(--color-light); filter: invert(100%);" else ""

  innerHTML =
    [ dataId id <> "{ padding: " <> padding <> "; border: " <> borderWidth <> " solid; " <> invertLine <> "}"
    , dataId id <> "{ background-color: inherit; }"
    ]

------------------------------------------------------------------------------
-- | Stack section
type StackConfig'
  = ( space :: String
    , recursive :: Boolean
    , splitAfter :: Maybe Int
    )

type StackConfig
  = Record StackConfig'

defaultStackConfig :: StackConfig
defaultStackConfig =
  { space: "var(--s1)"
  , recursive: false
  , splitAfter: Nothing
  }

stack :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack children = stack_ (fromRecord {}) children

stack_ :: forall a. Option StackConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "stack-l")
    [ _data { "i": id }
    ]
    children
  where
  space = fromMaybe defaultStackConfig.space (get (SProxy :: _ "space") overrides)

  recursive = fromMaybe defaultStackConfig.recursive (get (SProxy :: _ "recursive") overrides)

  splitAfter = fromMaybe defaultStackConfig.splitAfter (get (SProxy :: _ "splitAfter") overrides)

  id = "Stack-" <> space <> show recursive <> maybe "" show splitAfter

  recursiveCombinator = if recursive then " " else " > "

  -- #id
  -- [data-i=id]
  innerHTML =
    [ dataId id <> recursiveCombinator <> "* + * { margin-top: " <> space <> "}"
    ]
      <> conditionalHTML

  conditionalHTML = case splitAfter of
    Just n ->
      [ dataId id <> ":only-child{ height: 100%; }"
      , dataId id <> " > :nth-child(" <> show n <> ") { margin-bottom: auto; }"
      ]
    Nothing -> []

dataId id = "[data-i=\"" <> id <> "\"]"
