{-|

Module      : Control.PID
Description : provides PID control loop functionality
Copyright   : Jonathan Lamothe
License     : LGPL-3
Maintainer  : jlamothe1980@gmail.com

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

{-# LANGUAGE TemplateHaskell #-}

module Control.PID
  ( Settings (..)
  , Status (..)
  , setpoint
  , pFactor
  , iFactor
  , dFactor
  , bias
  , isReversed
  , maxOutput
  , settings
  , lastError
  , lastIntegral
  , newSettings
  , newStatus
  , run
  , resetIntegral
  ) where

import Control.Lens

-- | PID control loop settings
data Settings a =
  Settings
  { _setpoint  :: a
    -- ^ the desired setpoint
  , _pFactor   :: a
    -- ^ the proportional factor
  , _iFactor   :: a
    -- ^ the integral factor
  , _dFactor   :: a
    -- ^ the derivitave factor
  , _bias      :: a
    -- ^ the output bias
  , _isReversed  :: Bool
    -- ^ indicates whether or not the PID output is reversed
  , _maxOutput :: a
    -- ^ the maximum PID output
  } deriving (Eq, Show)

-- | PID status
data Status a =
  Status
  { _settings     :: Settings a
    -- ^ the PID loop's settings
  , _lastError    :: a
    -- ^ the error on the last run of the PID loop
  , _lastIntegral :: a
    -- ^ the accumulated integral (without factor)
  } deriving (Eq, Show)

makeLenses ''Settings
makeLenses ''Status

-- | the default PID settings
-- These will probably need tuning.
newSettings :: Fractional a => Settings a
newSettings = Settings 0 1 1 1 0 False 100

-- | an inital PID state with the default PID settings
newStatus :: Fractional a => Status a
newStatus = Status newSettings 0 0

-- | runs the PID loop
run
  :: (Ord n, Fractional n)
  => n
     -- ^ the amount of time elapsed since the PID loop was last run
  -> n
     -- ^ the current input value
  -> Status n
     -- ^ the current PID status
  -> (n, Status n)
     -- ^ the output and updated PID status
run dt x s = (out', s') where
  out' = max 0 $ min (s^.settings.maxOutput) out
  out = if s^.settings.isReversed
    then b - p - i - d
    else b + p + i + d
  s' = over lastIntegral (\x -> x + dt * err) $ set (lastError) err s
  b = s^.settings.bias
  p = (s^.settings.pFactor) * err
  i = (s^.settings.iFactor) * (s'^.lastIntegral)
  d = if dt == 0
    then 0
    else (s^.settings.dFactor) * (err - (s^.lastError)) / dt
  err = x - (s^.settings.setpoint)

-- | resets the integral of a PID loop
resetIntegral
  :: Num n
  => Status n
     -- ^ the PID status being reset
  -> Status n
     -- ^ the updated PID status
resetIntegral = set lastIntegral 0

--jl
