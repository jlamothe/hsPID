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

run :: Fractional n => n -> n -> Status n -> (n, Status n)
run = undefined

--jl
