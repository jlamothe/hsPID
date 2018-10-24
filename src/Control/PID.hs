{-# LANGUAGE TemplateHaskell #-}

module Control.PID
    ( Settings
    , Status
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
    ) where

import Control.Lens

data Settings a =
  Settings
  { _setpoint  :: a
  , _pFactor   :: a
  , _iFactor   :: a
  , _dFactor   :: a
  , _bias      :: a
  , _isReversed  :: Bool
  , _maxOutput :: a
  } deriving (Eq, Show)

data Status a =
  Status
  { _settings     :: Settings a
  , _lastError    :: a
  , _lastIntegral :: a
  } deriving (Eq, Show)

makeLenses ''Settings
makeLenses ''Status

newSettings :: Fractional a => Settings a
newSettings = Settings 0 1 1 1 0 False 100

newStatus :: Fractional a => Status a
newStatus = Status newSettings 0 0

run :: (Eq n, Fractional n) => n -> n -> Status n -> (n, Status n)
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

--jl
