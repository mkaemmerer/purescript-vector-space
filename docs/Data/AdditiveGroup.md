## Module Data.AdditiveGroup

#### `AdditiveGroup`

``` purescript
class AdditiveGroup v where
  zeroV :: v
  addV :: v -> v -> v
  negateV :: v -> v
```

Additive groups should satisfy these laws

- Associativity: `x ^+^ (y ^+^ z) = (x ^+^ y) ^+^ z`
- Commutativity: `x ^+^ y = y ^+^ x`
- Right Identity: `x ^+^ zeroV = x`
- Left Identity: `zeroV ^+^ x = x`
- Inverse: `x ^+^ negateV x = negateV x ^+^ x = zeroV`


##### Instances
``` purescript
instance additiveGroupUnit :: AdditiveGroup Unit
instance additiveGroupRing :: (Ring s) => AdditiveGroup s
instance additiveGroupTuple :: (AdditiveGroup u, AdditiveGroup v) => AdditiveGroup (Tuple u v)
instance additiveGroupArr :: (AdditiveGroup v) => AdditiveGroup (u -> v)
instance additiveGroupVec2 :: (AdditiveGroup v) => AdditiveGroup (Vec (Suc (Suc Zero)) v)
instance additiveGroupVec3 :: (AdditiveGroup v) => AdditiveGroup (Vec (Suc (Suc (Suc Zero))) v)
instance additiveGroupVec4 :: (AdditiveGroup v) => AdditiveGroup (Vec (Suc (Suc (Suc (Suc Zero)))) v)
```

#### `(^+^)`

``` purescript
(^+^) :: forall v. (AdditiveGroup v) => v -> v -> v
```

_left-associative / precedence 6_

Alias for `addV`

#### `(^-^)`

``` purescript
(^-^) :: forall v. (AdditiveGroup v) => v -> v -> v
```

_left-associative / precedence 6_

Group subtraction

#### `sumV`

``` purescript
sumV :: forall f v. (Foldable f, AdditiveGroup v) => f v -> v
```

Sum over several vectors


