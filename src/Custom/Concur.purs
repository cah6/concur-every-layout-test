module Custom.Concur
  ( module Concur.Core
  , module Concur.Core.Types
  , module Concur.React
  , module Concur.React.DOM
  , module Concur.React.Props
  , module Concur.React.Run
  , module Control.MultiAlternative
  , module React.DOM
  ) where

import Concur.Core (Widget)
import Concur.Core.Types (andd)
import Concur.React (HTML)
import Concur.React.DOM hiding (style)
import Concur.React.Props (onClick, className, size, style, unsafeMkProp, role, src, height, width, classList, classID)
import Concur.React.Run (runWidgetInDom)
import Control.MultiAlternative (orr)
import React.DOM (mkDOM, IsDynamic(..))
