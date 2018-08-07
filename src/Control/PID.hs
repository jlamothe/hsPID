module Control.PID
    ( PIDSettings (..)
    , PIDStatus
    , newPIDSettings
    , newPIDStatus
    , HasPIDSettings
    ) where

data PIDSettings a =
  PIDSettings
  { pidSetpoint      :: a
  , pidPFactor       :: a
  , pidIFactor       :: a
  , pidDFactor       :: a
  , pidBias          :: a
  , pidReversed      :: Bool
  , pidMaxOutput     :: a
  , pidIntegralLimit :: a
  } deriving (Eq, Show)

data PIDStatus a =
  PIDStatus
  { pidSettings     :: PIDSettings a
  , pidLastError    :: a
  , pidLastIntegral :: a
  } deriving (Eq, Show)

newPIDSettings :: Fractional a => PIDSettings a
newPIDSettings = PIDSettings 0 1 1 1 0 False 100 100

newPIDStatus :: Fractional a => PIDStatus a
newPIDStatus = PIDStatus newPIDSettings 0 0

class HasPIDSettings t where
  getPIDSettings :: t a -> PIDSettings a
  setPIDSettings :: PIDSettings a -> t a -> t a

class HasPIDStatus t where
  getPIDStatus :: t a -> PIDStatus a
  setPIDStatus :: PIDStatus a -> t a -> t a

instance HasPIDSettings PIDSettings where
  getPIDSettings     = id
  setPIDSettings x _ = x

instance HasPIDSettings PIDStatus where
  getPIDSettings = pidSettings
  setPIDSettings x s = s { pidSettings = x }

instance HasPIDStatus PIDStatus where
  getPIDStatus = id
  setPIDStatus x _ = x

changePIDSettings
  :: HasPIDSettings t
  => (PIDSettings a -> PIDSettings a)
  -> t a
  -> t a
changePIDSettings f x = setPIDSettings (f $ getPIDSettings x) x
