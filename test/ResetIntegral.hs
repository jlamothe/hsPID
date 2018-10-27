module ResetIntegral (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests = TestLabel "resetIntegral" $ (s'^.lastIntegral) ~?= 0 where
  s' = resetIntegral s
  s = set lastIntegral 1 newStatus

-- jl
