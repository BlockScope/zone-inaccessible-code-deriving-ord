module Flight.Zone.Zone where

import Data.Foldable (asum)
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import Data.UnitsOfMeasure (u, fromRational', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (fromQ)
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), fromDMS)
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.Incline (Incline(..), QIncline)
import qualified Flight.Zone.Raw.Zone as Raw (RawZone(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))

-- | Does it have area?
class HasArea a where
    hasArea :: a -> Bool

data Goal
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data EndOfSpeedSection
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data Turnpoint
    deriving (AnyZone, ZoneMaybeCylindrical, EssAllowedZone, GoalAllowedZone)

data CourseLine
    deriving (AnyZone, ZoneMaybeCylindrical)

data OpenDistance
    deriving AnyZone

class AnyZone a where
class ZoneMaybeCylindrical a where
class EssAllowedZone a where
class GoalAllowedZone a where

-- TODO: Remove standalone deriving Eq & Ord for empty data after GHC 8.4.1
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/7401
deriving instance Eq Goal
deriving instance Eq EndOfSpeedSection
deriving instance Eq Turnpoint
deriving instance Eq CourseLine
deriving instance Eq OpenDistance

deriving instance Ord Goal
deriving instance Ord EndOfSpeedSection
deriving instance Ord Turnpoint
deriving instance Ord CourseLine
deriving instance Ord OpenDistance

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone k a where
    Point
        :: (Eq a, Ord a)
        => Zone CourseLine a

    Vector
        :: (Eq a, Ord a)
        => QBearing a [u| rad |]
        -> Zone OpenDistance a

    Conical
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> Zone EndOfSpeedSection a

deriving instance Eq (Zone k a)
deriving instance Ord (Zone k a)
deriving instance
    ( Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    )
    => Show (Zone k a)

