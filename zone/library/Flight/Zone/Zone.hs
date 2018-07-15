module Flight.Zone.Zone
    ( HasArea(..), Zone(..), RawZoneToZone
    , center, radius, showZoneDMS, rawZonesToZones
    ) where

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
    -- | Used to mark the exact turnpoints in the optimized task distance.
    Point
        :: (Eq a, Ord a)
        => LatLng a [u| rad |]
        -> Zone CourseLine a

    -- | Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    Vector
        :: (Eq a, Ord a)
        => QBearing a [u| rad |]
        -> LatLng a [u| rad |]
        -> Zone OpenDistance a

    -- | The turnpoint cylinder.
    Cylinder
        :: (Eq a, Ord a, ZoneMaybeCylindrical k)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone k a

    -- | Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    Conical
        :: (Eq a, Ord a)
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone EndOfSpeedSection a

    -- | A goal line perpendicular to the course line.
    Line 
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone Goal a

    -- | This like a cylinder control zone but only used for goal.
    Circle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone Goal a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle
        :: (Eq a, Ord a)
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone Goal a

deriving instance Eq (Zone k a)
deriving instance Ord (Zone k a)
deriving instance
    ( Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    , Show (LatLng a [u| rad |])
    )
    => Show (Zone k a)

instance (Ord a, Num a) => HasArea (Zone k a) where
    hasArea (Point _) = False
    hasArea (Vector _ _) = False
    hasArea (Cylinder (Radius x) _) = x > zero
    hasArea (Conical _ (Radius x) _) = x > zero
    hasArea (Line (Radius x) _) = x > zero
    hasArea (Circle (Radius x) _) = x > zero
    hasArea (SemiCircle (Radius x) _) = x > zero

instance
    ( ToJSON (LatLng a [u| rad |])
    , ToJSON (QBearing a [u| rad |])
    , ToJSON (QIncline a [u| rad |])
    , ToJSON (QRadius a [u| m |])
    )
    => ToJSON (Zone k a) where
    toJSON (Point x) = object
        [ "point" .= object
            [ "latlng" .= toJSON x
            ]
        ]
    toJSON (Vector b x) = object
        [ "vector" .= object
            [ "bearing" .= toJSON b
            , "latlng" .= toJSON x
            ]
        ]
    toJSON (Cylinder r x) = object
        ["cylinder" .= object
            [ "radius" .= toJSON r
            , "latlng" .= toJSON x
            ]
        ]
    toJSON (Conical i r x) = object
        [ "conical" .= object
            [ "radius" .= toJSON r
            , "incline" .= toJSON i
            , "latlng" .= toJSON x
            ]
        ]
    toJSON (Line r x) = object
        [ "line" .= object
            [ "radius" .= toJSON r
            , "latlng" .= toJSON x
            ]
        ]
    toJSON (Circle r x) = object
        [ "circle" .= object
            [ "radius" .= toJSON r
            , "latlng" .= toJSON x
            ]
        ]

    toJSON (SemiCircle r x) = object
        [ "semicircle" .= object
            [ "radius" .= toJSON r
            , "latlng" .= toJSON x
            ]
        ]

instance
    ( Eq a
    , Ord a
    , FromJSON (LatLng a [u| rad |])
    , FromJSON (QBearing a [u| rad |])
    , FromJSON (QIncline a [u| rad |])
    , FromJSON (QRadius a [u| m |])
    )
    => FromJSON (Zone k a) where
    parseJSON = withObject "Zone" $ \o ->
        asum
            [ do
                o .: "point"

            , do
                o .: "vector"

            , do
                o .: "cylinder"

            , do
                o .: "conical"

            , do
                o .: "line"

            , do
                o .: "circle"

            , do
                o .: "semicircle"

            , fail $ "Unknown type of zone "
            ]

                {-Point <$> pt .: "x" -}
                {-Vector <$> vc .: "b" <*> vc .: "x"-}
                {-Cylinder <$> cy .: "r" <*> cy .: "x"-}
                {-Conical <$> co .: "r" <*> co .: "i" <*> co .: "x"-}
                {-Line <$> ln .: "r" <*> ln .: "x"-}
                {-Circle <$> cc .: "r" <*> cc .: "x"-}
                {-SemiCircle <$> sc .: "r" <*> sc .: "x"-}

showZoneDMS :: Zone k Double -> String
showZoneDMS (Point (LatLng (Lat x, Lng y))) =
    "Point " ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Bearing b) (LatLng (Lat x, Lng y))) =
    "Vector " ++ show (fromQ b) ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Cylinder r (LatLng (Lat x, Lng y))) =
    "Cylinder " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Conical (Incline i) r (LatLng (Lat x, Lng y))) =
    "Conical "
    ++ show (fromQ i)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

showZoneDMS (Line r (LatLng (Lat x, Lng y))) =
    "Line " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Circle r (LatLng (Lat x, Lng y))) =
    "Circle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (SemiCircle r (LatLng (Lat x, Lng y))) =
    "SemiCircle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

-- | The effective center point of a zone.
center :: Zone k a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (Circle _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => Zone k a -> QRadius a [u| m |]
radius (Point _) = Radius [u| 0m |]
radius (Vector _ _) = Radius [u| 0m |]
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (Circle r _) = r
radius (SemiCircle r _) = r

data RaceTask
data OpenDistanceTask

data TaskZones k a where

    EssIsGoal
        :: GoalAllowedZone g
        => ([Zone Turnpoint a], Zone g a)
        -> TaskZones RaceTask a

    EssIsNotGoal
        :: (EssAllowedZone e, GoalAllowedZone g)
        =>
            ( [Zone Turnpoint a]
            , Zone e a
            , [Zone Turnpoint a]
            , Zone g a
            )
        -> TaskZones RaceTask a

    OpenDistanceTaskZones
        :: ([Zone Turnpoint a], Zone OpenDistance a)
        -> TaskZones OpenDistance a

type RawZoneToZone k
    = GoalAllowedZone k
    => QRadius Double [u| m |]
    -> LatLng Double [u| rad |]
    -> TaskZones k Double

{-rawZonesToZones-}
    {-:: RawZoneToZone k-}
    {--> [Raw.RawZone]-}
    {--> Maybe (TaskZones k Double)-}
rawZonesToZones goal xs =
    case reverse xs of
        (x : xs) ->
            Just $ EssIsGoal (reverse $ f Cylinder <$> xs, f goal x)
        _ -> Nothing
    where
        f ctor Raw.RawZone{radius = r, lat, lng} =
            ctor r $ fromDMS (fromQ qLat, fromQ qLng)
                where
                    RawLat lat' = lat
                    RawLng lng' = lng

                    qLat :: Quantity Double [u| deg |]
                    qLat = fromRational' $ MkQuantity lat'

                    qLng :: Quantity Double [u| deg |]
                    qLng = fromRational' $ MkQuantity lng'
