module Main where

import Custom.Prelude
import Option
import Concur.React.Props (_id)
import Custom.Concur as C
import DOM (installStyleInHead)

main :: Effect Unit
main = C.runWidgetInDom "root" hello

hello :: forall a. C.Widget C.HTML a
hello = do
  _ <-
    stack_ (fromRecord { space: "2rem" })
      [ C.h1' [ C.text "A header!" ]
      , C.h2' [ C.text "Second header coming up!" ]
      , C.h3' [ C.text "Lorem ipsum" ]
      ]
  C.text "Hello!"

type StackConfig'
  = ( space :: String
    , recursive :: Boolean
    , splitAfter :: Maybe Int
    )

type StackConfig
  = Record StackConfig'

stack :: forall a. (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack children = stack_ (fromRecord {}) children

stack_ :: forall a. Option StackConfig' -> (Array (C.Widget C.HTML a)) -> C.Widget C.HTML a
stack_ overrides children = do
  _ <- liftEffect $ installStyleInHead id (intercalate "" innerHTML)
  C.el' (C.mkDOM (C.IsDynamic false) "stack-l")
    [ C.style
        { "--space": "1rem"
        , "--recursive": false
        , "--splitAfter": 1
        }
    , _id id
    ]
    children
  where
  space = fromMaybe defaultStackConfig.space (get (SProxy :: _ "space") overrides)

  recursive = fromMaybe defaultStackConfig.recursive (get (SProxy :: _ "recursive") overrides)

  splitAfter = fromMaybe defaultStackConfig.splitAfter (get (SProxy :: _ "splitAfter") overrides)

  id = "Stack-" <> space <> show recursive <> maybe "" show splitAfter

  recursiveCombinator = if recursive then " " else " > "

  innerHTML =
    [ "#" <> id <> recursiveCombinator <> "* + * { margin-top: " <> space <> "}"
    ]
      <> conditionalHTML

  conditionalHTML = case splitAfter of
    Just n ->
      [ "#" <> id <> ":only-child{ height: 100%; }"
      , "#" <> id <> " > :nth-child(" <> show n <> ") { margin-bottom: auto; }"
      ]
    Nothing -> []

defaultStackConfig :: StackConfig
defaultStackConfig =
  { space: "1rem"
  , recursive: false
  , splitAfter: Nothing
  }
