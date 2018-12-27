module Main where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)

type PortName = String
foreign import data ElmApp :: Type
foreign import elmFullScreenImpl :: Fn3 Foreign (ElmApp -> Maybe ElmApp) (Maybe ElmApp) (Effect (Maybe ElmApp))
foreign import elmSubscribeImpl :: Fn3 PortName ElmApp (Foreign -> Effect Unit) (Effect Unit)

elmFullScreen :: Foreign ->  Effect (Maybe ElmApp)
elmFullScreen flag = runFn3 elmFullScreenImpl flag Just Nothing

elmSubscribe :: 
  forall a. PortName -> ElmApp ->  (a -> Effect Unit) -> Effect Unit
elmSubscribe p e c = runFn3 elmSubscribeImpl p e callback
  where
    callback = c <<< unsafeFromForeign 

type Gains =
    { headGain :: Int
    , eighthGain :: Int
    , sixteenthGain :: Int
    , tripletsGain :: Int
    }

main :: Effect Unit
main = do
  mayElm <- elmFullScreen $ unsafeToForeign false
  case mayElm of
    Nothing ->
      log "no elm"
    Just elm  ->
      traceM elm
