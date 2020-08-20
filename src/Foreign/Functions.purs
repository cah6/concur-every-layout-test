module Foreign.Functions where

import Custom.Prelude
import React.Ref (NativeNode)

foreign import toggleOverflowClass :: NativeNode -> Effect Unit

foreign import attachReelObserver :: NativeNode -> Effect Unit
