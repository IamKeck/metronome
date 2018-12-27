module Audio where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Effect (Effect)

foreign import data AudioContext :: Type
foreign import data GainNode :: Type
foreign import data Oscillator :: Type
foreign import getAudioContext :: Effect AudioContext
foreign import createGain :: AudioContext -> Effect GainNode
foreign import gainConnectToContextImpl :: Fn3 GainNode AudioContext Unit (Effect Unit)
foreign import gainConnectToGainImpl :: Fn3 GainNode GainNode Unit (Effect Unit)
foreign import createOscillator :: AudioContext -> (Effect Oscillator)
foreign import oscillatorConnectToGainImpl :: Fn3 Oscillator GainNode Unit (Effect Unit)
foreign import startOscillatorImpl :: Fn2 Oscillator Unit (Effect Unit)
foreign import stopOscillatorImpl :: Fn2 Oscillator Unit (Effect Unit)
foreign import setGainValueImpl :: Fn3 Int GainNode Unit (Effect Unit)

gainConnectToContext :: GainNode -> AudioContext -> Effect Unit
gainConnectToContext g a = runFn3 gainConnectToContextImpl g a unit

gainConnectToGain :: GainNode -> GainNode -> Effect Unit
gainConnectToGain g g2 = runFn3 gainConnectToGainImpl g g2 unit

oscillatorConnectToGain :: Oscillator -> GainNode -> Effect Unit
oscillatorConnectToGain o g = runFn3 oscillatorConnectToGainImpl o g unit

startOscillator :: Oscillator -> Effect Unit
startOscillator o = runFn2 startOscillatorImpl o unit

stopOscillator :: Oscillator -> Effect Unit
stopOscillator o = runFn2 stopOscillatorImpl o unit

setGainValue :: Int -> GainNode -> Effect Unit
setGainValue v g = runFn3 setGainValueImpl v g unit