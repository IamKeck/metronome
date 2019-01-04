module Main where

import Prelude

import Audio (AudioContext, GainNode, createGain, createOscillator, gainConnectToContext, gainConnectToGain, getAudioContext, getCurrentTime, oscillatorConnectToGain, setGainValue, startOscillator, stopOscillator)
import Data.Array (cons, head, last, reverse, singleton, tail)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, modify, new, read, write)
import Effect.Timer (IntervalId, clearInterval, setInterval)
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
    callback v = c $ unsafeFromForeign v

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


-- Metronome Player

type Metronome = {
  ctx :: AudioContext,
  mainGain :: GainNode,
  headNoteGain :: GainNode,
  eightNoteGain :: GainNode,
  sixteenthNoteGain :: GainNode,
  tripletsNoteGain :: GainNode,
  lastSetHeadNote ::  Number,
  interval ::  Interval,
  timerInterval ::  TimerInterval,
  timer :: Maybe IntervalId
}

createMetronome :: Effect Metronome
createMetronome = do
  ctx <- getAudioContext
  mainGain <- createGain ctx
  setGainValue 1.0 mainGain
  headNoteGain <- createGain ctx
  eightNoteGain <- createGain ctx
  sixteenthNoteGain <- createGain ctx
  tripletsNoteGain <- createGain ctx
  gainConnectToGain headNoteGain mainGain
  gainConnectToGain eightNoteGain mainGain
  gainConnectToGain sixteenthNoteGain mainGain
  gainConnectToGain tripletsNoteGain mainGain
  gainConnectToContext mainGain ctx
  pure {
    ctx : ctx,
    mainGain : mainGain,
    headNoteGain : headNoteGain,
    eightNoteGain : eightNoteGain,
    sixteenthNoteGain : sixteenthNoteGain,
    tripletsNoteGain : tripletsNoteGain,
    lastSetHeadNote : 0.0,
    interval : 0.0 ,
    timerInterval : 2,
    timer : Nothing 
  }

setNote :: Metronome -> Time -> GainNode -> Effect Unit
setNote m t g = do
  o <- createOscillator m.ctx
  oscillatorConnectToGain o g
  startOscillator t o
  stopOscillator (t + length) o
  pure unit
  where
    length = 0.1

getLastHeadNote :: Array BeatsInBeat -> Maybe Time
getLastHeadNote beats = do
    lastBeats <- last beats
    head lastBeats.head

setNotes :: Time -> Metronome -> Effect Metronome
setNotes from m = do
    currentTime <- getCurrentTime m.ctx
    let until = currentTime + (toNumber m.timerInterval)
    let times = getRange from until m.interval
    let newLastSetHeadNote = fromMaybe m.lastSetHeadNote $ getLastHeadNote times
    let newM = m {lastSetHeadNote = newLastSetHeadNote}
    _ <- for times \time -> do
        _ <- for time.head \time' ->
            setNote m time' m.headNoteGain
        _ <- for time.eighth \time' ->
            setNote m time' m.eightNoteGain
        _ <- for time.sixteenth \time' ->
            setNote m time' m.sixteenthNoteGain
        _ <- for time.triplets \time' ->
            setNote m time' m.tripletsNoteGain
        pure unit

    pure newM

setNewInterval :: Interval -> Metronome -> Metronome
setNewInterval i m = m {interval = i, timerInterval = getTimerInterval i}

start :: Ref Metronome -> Effect Unit
start mr = do
  m <- read mr
  setGainValue 1.0 m.mainGain
  currentTime <- getCurrentTime m.ctx
  newM <- setNotes (currentTime - m.interval) m
  write newM mr
  intervalId <- setInterval m.timerInterval $ do
    m' <- read mr
    newM' <- setNotes m'.lastSetHeadNote m'
    write newM' mr
  _ <- modify (\m' -> m' {timer = Just intervalId}) mr
  pure unit

stop :: Metronome -> Effect Metronome
stop m = do
    setGainValue 0.0 m.mainGain
    case m.timer of
        Nothing -> pure unit
        Just i -> clearInterval i
    pure $ m {timer = Nothing}

toggle :: Ref Metronome -> Effect Unit
toggle mr = do 
    m <- read mr
    case m.timer of
        Nothing -> start mr
        Just _ -> do
            newM <- stop m
            write newM mr

setNewGains :: Gains -> Metronome -> Effect Unit
setNewGains g m = do
    pure unit
    setGainValue (toNumber g.headGain / 100.0) m.headNoteGain
    setGainValue (toNumber g.eighthGain / 100.0) m.eightNoteGain
    setGainValue (toNumber g.sixteenthGain / 100.0) m.sixteenthNoteGain
    setGainValue (toNumber g.tripletsGain / 100.0) m.tripletsNoteGain


main :: Effect Unit
main = do

  mayElm <- elmFullScreen $ unsafeToForeign false
  case mayElm of
    Nothing ->
      log "no elm"
    Just elm -> do
      m <- createMetronome
      mr <- new m :: Effect (Ref Metronome)
      elmSubscribe "newInterval" elm $ \i -> do
        log "newint"
        m' <- read mr
        let nm = setNewInterval i m' :: Metronome
        write nm mr

      elmSubscribe "toggle" elm $ \i -> do
        m' <- read mr
        let nm = setNewInterval i m'
        write nm mr
        toggle mr
    
      elmSubscribe "newGains" elm $ \g -> do
        log "newGain"
        m' <- read mr
        setNewGains g m'
