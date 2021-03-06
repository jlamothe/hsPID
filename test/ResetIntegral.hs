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

module ResetIntegral (tests) where

import Control.Lens
import Test.HUnit

import Control.PID

tests = TestLabel "resetIntegral" $ (s'^.lastIntegral) ~?= 0 where
  s' = resetIntegral s
  s = set lastIntegral 1 newStatus

-- jl
