module Settings where

import Control.Lens
import Test.HUnit

import Control.PID

tests :: Test
tests = TestLabel "Settings" $ TestList
  [ newSettingsTests
  ]

newSettingsTests :: Test
newSettingsTests = TestLabel "newSettings" $ TestList $
  [ TestLabel "setpoint" $   settings^.setpoint   ~?= 0
  , TestLabel "pFactor" $    settings^.pFactor    ~?= 1
  , TestLabel "pidIFactor" $ settings^.iFactor    ~?= 1
  , TestLabel "dFactor" $    settings^.dFactor    ~?= 1
  , TestLabel "bias" $       settings^.bias       ~?= 0
  , TestLabel "reversePID" $ settings^.reversePID ~?= False
  , TestLabel "maxOutput" $  settings^.maxOutput  ~?= 100
  ]

settings = newSettings :: Settings Rational
