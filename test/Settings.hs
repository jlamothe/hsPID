module Settings where

import Control.Lens
import Test.HUnit

import qualified Control.PID as PID

tests :: Test
tests = TestLabel "Settings" $ TestList
  [ newSettingsTests
  ]

newSettingsTests :: Test
newSettingsTests = TestLabel "newSettings" $ TestList $
  [ TestLabel "setpoint" $   new^.PID.setpoint  ~?= 0
  , TestLabel "pFactor" $    new^.PID.pFactor   ~?= 1
  , TestLabel "pidIFactor" $ new^.PID.iFactor   ~?= 1
  , TestLabel "dFactor" $    new^.PID.dFactor   ~?= 1
  , TestLabel "bias" $       new^.PID.bias      ~?= 0
  , TestLabel "reversePID" $ new^.PID.reversed  ~?= False
  , TestLabel "maxOutput" $  new^.PID.maxOutput ~?= 100
  ]

new = PID.newSettings :: PID.Settings Rational
