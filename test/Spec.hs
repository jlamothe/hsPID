{-

hsPID
Copyright (C) 2018 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program.  If not, see
<https://www.gnu.org/licenses/>.

-}

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
