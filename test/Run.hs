module Run (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests = TestLabel "run" $ TestList [propTests, intTests]

propTests = TestLabel "proportional" $ TestList $ map propTest
  --  input setpoint factor bias reversed expected
  [ ( 0,    0,       1,     0,   False,   0        )
  , ( 1,    0,       1,     0,   False,   1        )
  , ( 2,    1,       1,     0,   False,   1        )
  , ( 1,    0,       2,     0,   False,   2        )
  , ( 0,    0,       1,     50,  False,   50       )
  , ( 1,    0,       1,     50,  False,   51       )
  , ( 1,    0,       2,     50,  False,   52       )
  , ( -1,   0,       1,     50,  False,   49       )
  , ( 0,    0,       1,     100, False,   100      )
  , ( -1,   0,       1,     100, False,   99       )
  , ( 0,    0,       1,     0,   True,    0        )
  , ( -1,   0,       1,     0,   True,    1        )
  , ( 0,    0,       1,     50,  True,    50       )
  , ( -1,   0,       1,     50,  True,    51       )
  , ( 1,    0,       1,     50,  True,    49       )
  , ( 0,    0,       1,     100, True,    100      )
  , ( 1,    0,       1,     100, True,    99       )
  ]

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

intTests = TestLabel "integral" $ TestList $ map intTest
  --  time1 input1 time2 input2 setpoint factor bias reversed expected
  [ ( 1,    0,     1,    0,     0,       1,     0,   False,   0        )
  , ( 1,    1,     1,    1,     0,       1,     0,   False,   2        )
  , ( 1,    2,     1,    2,     1,       1,     0,   False,   2        )
  , ( 1,    1,     2,    1,     0,       1,     0,   False,   3        )
  , ( 1,    2,     1,    1,     0,       1,     0,   False,   3        )
  , ( 2,    1,     2,    1,     0,       1,     0,   False,   4        )
  , ( 1,    1,     1,    1,     0,       2,     0,   False,   4        )
  , ( 1,    1,     1,    1,     0,       1,     50,  False,   52       )
  , ( 1,    1,     1,    1,     0,       1,     50,  True,    48       )
  , ( 1,    1,     1,    1,     0,       1,     100, True,    98       )
  ]

intTest (t1, x1, t2, x2, sp, k, b, r, expect) =
  TestLabel label $ out ~?= expect where
  label = "time1=" ++ show t1 ++
    ", input1=" ++ show x1 ++
    ", time2=" ++ show t2 ++
    ", input2=" ++ show x2 ++
    ", setpoint=" ++ show sp ++
    ", factor=" ++ show k ++
    ", bias=" ++ show b ++
    ", reversed=" ++ show r
  (out, _) = run t2 x2 s2
  (_, s2) = run t1 x1 s1
  s1 = set (settings.setpoint) sp $
    set (settings.pFactor) 0 $
    set (settings.iFactor) k $
    set (settings.dFactor) 0 $
    set (settings.bias) b $
    set (settings.isReversed) r
    newStatus

--jl
