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
  [ TestLabel "setpoint" $   new^.setpoint   ~?= 0
  , TestLabel "pFactor" $    new^.pFactor    ~?= 1
  , TestLabel "pidIFactor" $ new^.iFactor    ~?= 1
  , TestLabel "dFactor" $    new^.dFactor    ~?= 1
  , TestLabel "bias" $       new^.bias       ~?= 0
  , TestLabel "isReversed" $ new^.isReversed ~?= False
  , TestLabel "maxOutput" $  new^.maxOutput  ~?= 100
  ]

new = newSettings :: Settings Rational
