module Main where

import Prelude

import Data.Array (cons, head, reverse, singleton, tail)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe)
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

type BeatsInBeat =
    { head :: Array Time,
      eighth :: Array Time,
      sixteenth :: Array Time,
      triplets :: Array Time
    }

type Interval = Number
type TimerInterval = Int
type Time = Number

-- pure functions

getTimerInterval :: Interval -> TimerInterval
getTimerInterval = ceil 

getBeatsInBar :: Time -> Interval -> BeatsInBeat
getBeatsInBar headNoteTime i = { head : singleton headNoteTime 
                               , eighth : singleton $ headNoteTime + i / 2.0
                               , sixteenth : [ headNoteTime + i / 4.0, headNoteTime + i / 4.0 * 3.0]
                               , triplets : [ headNoteTime + i / 3.0, headNoteTime + i / 3.0 * 2.0]
                               }

getRange :: Time -> Time -> Interval -> Array BeatsInBeat
getRange start stop interval = map (_ `getBeatsInBar` interval) headTimes
  where
    stopTime = stop - interval
    headTimes = fromMaybe [] <<< tail <<< reverse $ range start stopTime interval

range :: Number -> Number -> Number -> Array Number
range start stop interval = loop []
  where
    loop :: Array Number -> Array Number
    loop acc = case head acc of
      Nothing -> loop [start]
      Just h -> 
        let next = h + interval 
        in 
          if next <= stop then 
            loop $ cons next acc 
          else acc 

main :: Effect Unit
main = do
  mayElm <- elmFullScreen $ unsafeToForeign false
  case mayElm of
    Nothing ->
      log "no elm"
    Just elm  ->
      traceM elm
