{-# LANGUAGE TemplateHaskell #-}

module Control.PID
    ( PIDSettings (..)
    , PIDStatus
    , newPIDSettings
    , newPIDStatus
    ) where

import Control.Lens

data PIDSettings a =
  PIDSettings
  { _pidSetpoint      :: a
  , _pidPFactor       :: a
  , _pidIFactor       :: a
  , _pidDFactor       :: a
  , _pidBias          :: a
  , _pidReversed      :: Bool
  , _pidMaxOutput     :: a
  , _pidIntegralLimit :: a
  } deriving (Eq, Show)

data PIDStatus a =
  PIDStatus
  { _pidSettings     :: PIDSettings a
  , _pidLastError    :: a
  , _pidLastIntegral :: a
  } deriving (Eq, Show)

makeLenses ''PIDSettings
makeLenses ''PIDStatus

newPIDSettings :: Fractional a => PIDSettings a
newPIDSettings = PIDSettings 0 1 1 1 0 False 100 100

newPIDStatus :: Fractional a => PIDStatus a
newPIDStatus = PIDStatus newPIDSettings 0 0
