module Custom.Prelude
  ( module Prelude
  , module Control.Monad.Reader
  , module Data.Bifunctor
  , module Data.Const
  , module Data.Date
  , module Data.DateTime
  , module Data.Either
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Symbol
  , module Data.Tuple
  , module Effect
  , module Effect.Aff
  , module Effect.Aff.Class
  , module Effect.Class
  , module Effect.Console
  , module Foreign
  , module Prim.Boolean
  , undefined
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, class MonadTrans,
    Reader, ReaderT(..), ask, asks, lift, local, mapReader, mapReaderT, runReader,
    runReaderT, withReader, withReaderT)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Const (Const(..))
import Data.Date (Date, Day, Month(..), Weekday(..), Year, adjust, canonicalDate,
    day, diff, exactDate, isLeapYear, lastDayOfMonth, month, weekday, year)
import Data.DateTime (Date, DateTime(..))
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold,
    foldM, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr,
    foldrDefault, for_, indexl, indexr, intercalate, length, maximum, maximumBy,
    minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_,
    sum, surround, surroundMap, traverse_)
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft,
    isRight, note, note')
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing,
    maybe, maybe', optional)
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Data.Tuple (Tuple(..), curry, fst, lookup, snd, swap, uncurry)
import Effect (Effect, forE, foreachE, untilE, whileE)
import Effect.Aff (Aff, BracketConditions, Canceler(..), Error, Fiber, Milliseconds(..),
    ParAff, apathize, attempt, bracket, cancelWith, catchError, delay, effectCanceler,
    error, fiberCanceler, finally, forkAff, generalBracket, invincible, joinFiber,
    killFiber, launchAff, launchAff_, launchSuspendedAff, makeAff, message, never,
    nonCanceler, parallel, runAff, runAff_, runSuspendedAff, sequential, supervise,
    suspendAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, isArray, isNull,
    isUndefined, readArray, readBoolean, readChar, readInt, readNull, readNullOrUndefined,
    readNumber, readString, readUndefined, renderForeignError, tagOf, typeOf,
    unsafeFromForeign, unsafeReadTagged, unsafeToForeign)
import Prim.Boolean (False, True)
import Unsafe.Coerce (unsafeCoerce)


undefined :: forall a. a
undefined = unsafeCoerce "Not implemented"
