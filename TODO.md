* Perhaps generalize the Pretty pattern, so we can define some "canonical"
  pretty Fortran, then handle version gating in the Pretty typeclass.
* No mixing record and non-record syntax in a single constructor. And it's
  suggested not to do record syntax for sum types, because it makes selectors
  partial.
* I think we start by going "most AST nodes are heterogeneous lists, and start
  with an annotation followed by a `SrcSpan`"
* Apparently we *are* allowed to define mutually recursive instances via
  generics? Wow.

```haskell
data M1 = M1L M2 | M1R deriving (Generic, Show, Eq)
data M2 = M2L M1 | M2R deriving (Generic, Show, Eq)
instance ToJSON M1 where toJSON = genericToJSON $ jcSumDrop ""
instance ToJSON M2 where toJSON = genericToJSON $ jcSumDrop ""
```
