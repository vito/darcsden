{-# LANGUAGE StandaloneDeriving #-}
module DarcsDen.Data where

import Darcs.Patch (Prim)
import Data.Typeable (Typeable)
import Data.Data (Data)
import System.Time (CalendarTime(..), Month(..), Day(..))

deriving instance Typeable CalendarTime
deriving instance Data CalendarTime
deriving instance Typeable Month
deriving instance Data Month
deriving instance Typeable Day
deriving instance Data Day