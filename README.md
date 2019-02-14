# Fine Grained Stateful Computations

An experimental approach for representing stateful computations in Haskell, which
is polymorphic in both the effect-encapsulating type constructor `f` and the
constraint `c` imposed on `f`.

```
type Read  k f = forall a. k a -> f a

type Write k f = forall a. k a -> f a -> f a

type FS  c k a = forall f. c f => Read k f -> Write k f -> f a
```
