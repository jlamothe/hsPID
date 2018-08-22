import Control.Monad
import System.Exit
import Test.HUnit

import qualified Settings

main :: IO ()
main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0) exitFailure

tests :: Test
tests = TestList [Settings.tests]
