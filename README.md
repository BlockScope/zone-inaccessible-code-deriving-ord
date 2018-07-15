# `zone-inaccessible-code-deriving-ord`

Reproduction of the reported error;

```
/.../zone/library/Flight/Zone/Zone.hs:117:1: error:
    • Couldn't match type ‘OpenDistance’ with ‘CourseLine’
      Inaccessible code in
        a pattern with constructor:
          Point :: forall a.
                   (Eq a, Ord a) =>
                   LatLng a (Data.UnitsOfMeasure.Internal.MkUnit "rad")
                   -> Zone CourseLine a,
        in a case alternative
    • In the pattern: Point {}
      In a case alternative: Point {} -> GT
      In the expression:
        case b of
          Point {} -> GT
          Vector b1 b2
            -> case (compare a1 b1) of
                 LT -> LT
                 EQ -> (a2 `compare` b2)
                 GT -> GT
          _ -> LT
      When typechecking the code for ‘compare’
        in a derived instance for ‘Ord (Zone k a)’:
        To see the code I am typechecking, use -ddump-deriv
    |
117 | deriving instance Ord (Zone k a)
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
