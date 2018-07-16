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

-- NOTE: Below is the generated code from the dump with the following
-- replacements made.
-- :%s/GHC\.Classes\.//
-- :%s/GHC\.Types\.//
-- :%s/Flight\.Zone\.//
{-
instance (Eq a, Ord a) =>
           Ord (Zone k a) where
    compare a_a2hw b_a2hx
      = case a_a2hw of
          Point
            -> case b_a2hx of
                 Point -> EQ
                 _ -> LT
          Vector
            -> case b_a2hx of
                 Point {} -> GT
                 Vector -> EQ
                 _ -> LT
          Conical a1_a2hy
            -> case b_a2hx of
                 Conical b1_a2hz
                   -> (a1_a2hy `compare` b1_a2hz)
                 _ -> GT
    (<) a_a2hA b_a2hB
      = case a_a2hA of
          Point
            -> case b_a2hB of
                 Point -> False
                 _ -> True
          Vector
            -> case b_a2hB of
                 Point {} -> False
                 Vector -> False
                 _ -> True
          Conical a1_a2hC
            -> case b_a2hB of
                 Conical b1_a2hD -> (a1_a2hC < b1_a2hD)
                 _ -> False
    (<=) a_a2hE b_a2hF
      = not ((<) b_a2hF a_a2hE)
    (>) a_a2hG b_a2hH = (<) b_a2hH a_a2hG
    (>=) a_a2hI b_a2hJ
      = not ((<) a_a2hI b_a2hJ)
  
instance Eq a =>
           Eq (Zone k a) where
    (==) (Point) (Point)
      = True
    (==) (Vector) (Vector)
      = True
    (==)
      (Conical a1_a2hK)
      (Conical b1_a2hL)
      = ((a1_a2hK == b1_a2hL))
    (==) _ _ = False
    (/=) a_a2hM b_a2hN
      = not ((==) a_a2hM b_a2hN)
-}
