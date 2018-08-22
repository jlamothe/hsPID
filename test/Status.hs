module Status (tests) where

import Control.Lens
import Test.HUnit

import qualified Control.PID as PID

tests :: Test
tests = TestLabel "Status" $ TestList
  [ TestLabel "settings" $     new^.PID.settings     ~?= PID.newSettings
  , TestLabel "lastError" $    new^.PID.lastError    ~?= 0
  , TestLabel "lastIntegral" $ new^.PID.lastIntegral ~?= 0
  ]

new = PID.newStatus :: PID.Status Rational

--jl
