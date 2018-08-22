{-# LANGUAGE TemplateHaskell #-}

module Control.PID
    ( Settings
    , Status
    , setpoint
    , pFactor
    , iFactor
    , dFactor
    , bias
    , reversed
    , maxOutput
    , settings
    , lastError
    , lastIntegral
    , newSettings
    , newStatus
    ) where

import qualified Control.Lens as L

data Settings a =
  Settings
  { _setpoint  :: a
  , _pFactor   :: a
  , _iFactor   :: a
  , _dFactor   :: a
  , _bias      :: a
  , _reversed  :: Bool
  , _maxOutput :: a
  } deriving (Eq, Show)

data Status a =
  Status
  { _settings     :: Settings a
  , _lastError    :: a
  , _lastIntegral :: a
  } deriving (Eq, Show)

L.makeLenses ''Settings
L.makeLenses ''Status

newSettings :: Fractional a => Settings a
newSettings = Settings 0 1 1 1 0 False 100

newStatus :: Fractional a => Status a
newStatus = Status newSettings 0 0

--jl
