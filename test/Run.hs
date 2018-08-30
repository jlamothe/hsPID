module Run (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests :: Test
tests = TestLabel "run" $ TestList []
