# `zone-inaccessible-code-deriving-ord`

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
      In a case alternative: Point {} -> False
      In the expression:
        case b of
          Point {} -> False
          Vector -> False
          _ -> True
      When typechecking the code for ‘<’
        in a derived instance for ‘Ord (Zone k a)’:
        To see the code I am typechecking, use -ddump-deriv
```
