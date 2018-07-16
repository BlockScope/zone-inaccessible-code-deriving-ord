# Inaccessible code in a pattern with constructor

Reproduction of the reported error;

```
> stack build flight-zone --ghc-options=-ddump-deriv
...
/.../zone/library/Flight/Zone.hs:25:1: error:
    • Couldn't match type ‘OpenDistance’ with ‘CourseLine’
      Inaccessible code in
        a pattern with constructor:
          Point :: forall a. (Eq a, Ord a) => Zone CourseLine a,
        in a case alternative
    • In the pattern: Point {}
      In a case alternative: Point {} -> GT
      In the expression:
        case b of
          Point {} -> GT
          Vector -> EQ
          _ -> LT
      When typechecking the code for ‘compare’
        in a derived instance for ‘Ord (Zone k a)’:
        To see the code I am typechecking, use -ddump-deriv
   |
25 | deriving instance (Eq a, Ord a) => Ord (Zone k a)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

/.../zone/library/Flight/Zone.hs:25:1: error:
    • Couldn't match type ‘OpenDistance’ with ‘CourseLine’
      Inaccessible code in
        a pattern with constructor:
          Point :: forall a. (Eq a, Ord a) => Zone CourseLine a,
        in a case alternative
    • In the pattern: Point {}
      In a case alternative: Point {} -> False
      In the expression:
        case b of
          Point {} -> False
          Vector -> False
          _ -> True
      When typechecking the code for ‘<’
        in a derived instance for ‘Ord (Zone k a)’:
        To see the code I am typechecking, use -ddump-deriv
   |
25 | deriving instance (Eq a, Ord a) => Ord (Zone k a)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

The `*.cabal` file is ready to go but if you want to tweak any of the `*.dhall`
files from which it is generated then install the dhall tooling with;

```
> stack install dhall hpack-dhall
...
Copied executables to /.../__shake-build:
- dhall
- dhall-format
- dhall-hash
- dhall-repl
- hpack-dhall
```

To generate the `*.cabal` file;

```
> stack exec hpack-dhall -- zone
...
zone/flight-zone.cabal is up-to-date
```

Below is the generated code from the dump with the following
replacements made;

```
:%s/GHC\.Classes\.//
:%s/GHC\.Types\.//
:%s/Flight\.Zone\.//
```

```
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
```
