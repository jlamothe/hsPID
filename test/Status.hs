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
