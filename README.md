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

The cabal file is ready to go but if you want to tweak it using any of the
`*.dhall` files then install the dhall tooling with;

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
