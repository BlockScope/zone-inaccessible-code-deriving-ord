module Flight.Zone where

newtype Radius a = Radius a deriving (Eq, Ord)

data CourseLine
data OpenDistance
data EndOfSpeedSection

-- TODO: Remove standalone deriving Eq & Ord for empty data after GHC 8.4.1
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/7401
deriving instance Eq CourseLine
deriving instance Eq OpenDistance
deriving instance Eq EndOfSpeedSection

deriving instance Ord CourseLine
deriving instance Ord OpenDistance
deriving instance Ord EndOfSpeedSection

data Zone k a where
    Point :: (Eq a, Ord a) => Zone CourseLine a
    Vector :: (Eq a, Ord a) => Zone OpenDistance a
    Conical :: (Eq a, Ord a) => Radius a -> Zone EndOfSpeedSection a

deriving instance Eq a => Eq (Zone k a)
deriving instance (Eq a, Ord a) => Ord (Zone k a)
