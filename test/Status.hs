module Status (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests :: Test
tests = TestLabel "Status" $ TestList
  [ TestLabel "settings" $     new^.settings     ~?= newSettings
  , TestLabel "lastError" $    new^.lastError    ~?= 0
  , TestLabel "lastIntegral" $ new^.lastIntegral ~?= 0
  ]

new = newStatus :: Status Rational

--jl
