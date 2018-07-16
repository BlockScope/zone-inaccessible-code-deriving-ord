module Flight.Zone where

newtype Bearing a = Bearing a deriving (Eq, Ord, Show)
newtype Radius a = Radius a deriving (Eq, Ord, Show)

data EndOfSpeedSection
data CourseLine
data OpenDistance

-- TODO: Remove standalone deriving Eq & Ord for empty data after GHC 8.4.1
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/7401
deriving instance Eq EndOfSpeedSection
deriving instance Eq CourseLine
deriving instance Eq OpenDistance

deriving instance Ord EndOfSpeedSection
deriving instance Ord CourseLine
deriving instance Ord OpenDistance

data Zone k a where
    Point :: (Eq a, Ord a) => Zone CourseLine a
    Vector :: (Eq a, Ord a) => Bearing a -> Zone OpenDistance a
    Conical :: (Eq a, Ord a) => Radius a -> Zone EndOfSpeedSection a

deriving instance Eq (Zone k a)
deriving instance Ord (Zone k a)
deriving instance (Show (Bearing a), Show (Radius a)) => Show (Zone k a)
