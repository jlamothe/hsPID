import Control.Monad
import System.Exit
import Test.HUnit

import qualified ResetIntegral
import qualified Run
import qualified Settings
import qualified Status

main :: IO ()
main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0) exitFailure

tests :: Test
tests = TestList
  [ Settings.tests
  , Status.tests
  , Run.tests
  , ResetIntegral.tests
  ]

--jl
