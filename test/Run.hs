module Run (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests :: Test
tests = TestLabel "run" $ TestList [propTests]

propTests :: Test
propTests = TestLabel "proportional" $ TestList $ map propTest []

propTest
  :: (Rational, Rational, Rational, Rational, Bool, Rational)
  -> Test
propTest (x, sp, k, b, r, expect) =
  TestLabel label $ out ~?= expect where
  label = "input=" ++ show x ++
    ", setpoint=" ++ show sp ++
    ", factor=" ++ show k ++
    ", bias=" ++ show b ++
    ", reversed=" ++ show r
  (out, _) = run 1 x $
    set (settings.setpoint) sp $
    set (settings.pFactor) k $
    set (settings.iFactor) 0 $
    set (settings.dFactor) 0 $
    set (settings.bias) b $
    set (settings.isReversed) r
    newStatus

--jl
