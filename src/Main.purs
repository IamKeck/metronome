module Main where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign, unsafeFromForeign)

type PortName = String
foreign import data ElmApp :: Type
foreign import elmFullScreenImpl :: Fn2 (ElmApp -> Maybe ElmApp) (Maybe ElmApp) (Effect (Maybe ElmApp))
foreign import elmSubscribeImpl :: Fn3 PortName ElmApp (Foreign -> Effect Unit) (Effect Unit)

elmFullScreen :: Effect (Maybe ElmApp)
elmFullScreen = runFn2 elmFullScreenImpl Just Nothing

elmSubscribe :: 
  forall a. PortName -> ElmApp ->  (a -> Effect Unit) -> Effect Unit
elmSubscribe p e c = runFn3 elmSubscribeImpl p e callback
  where
    callback = c <<< unsafeFromForeign 


main :: Effect Unit
main = do
  mayElm <- elmFullScreen
  case mayElm of
    Nothing ->
      log "no elm"
    Just elm  ->
      traceM elm
