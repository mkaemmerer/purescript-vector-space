# Module Documentation

## Module Data.AdditiveGroup

#### `AdditiveGroup`

``` purescript
class AdditiveGroup v where
  zeroV :: v
  (^+^) :: v -> v -> v
  negateV :: v -> v
```

Additive groups should satisfy these laws

- Associativity: `x ^+^ (y ^+^ z) = (x ^+^ y) ^+^ z`
- Commutativity: `x ^+^ y = y ^+^ x`
- Right Identity: `x ^+^ zeroV = x`
- Left Identity: `zeroV ^+^ x = x`
- Inverse: `x ^+^ negateV x = negateV x ^+^ x = zeroV`


#### `(^-^)`

``` purescript
(^-^) :: forall v. (AdditiveGroup v) => v -> v -> v
```

Group subtraction

#### `sumV`

``` purescript
sumV :: forall f v. (Foldable f, AdditiveGroup v) => f v -> v
```

Sum over several vectors

#### `additiveGroupUnit`

``` purescript
instance additiveGroupUnit :: AdditiveGroup Unit
```


#### `additiveGroupRing`

``` purescript
instance additiveGroupRing :: (Ring s) => AdditiveGroup s
```


#### `additiveGroupTuple`

``` purescript
instance additiveGroupTuple :: (AdditiveGroup u, AdditiveGroup v) => AdditiveGroup (Tuple u v)
```



## Module Data.VectorSpace

#### `VectorSpace`

``` purescript
class (Ring s, AdditiveGroup v) <= VectorSpace v s where
  (*^) :: s -> v -> v
```

Vector spaces should satisfy these laws

- Identity: `one *^ v = v`
- Compatibility: `a * (b *^ v) = (a * b) *^ v`
- Distributivity1: `a *^ (u ^+^ v) = (a *^ u) ^+^ (a *^ v)`
- Distributivity2: `(a + b) *^ v = (a *^ v) + (b *^ v)`


#### `InnerSpace`

``` purescript
class (VectorSpace v s) <= InnerSpace v s where
  (<.>) :: v -> v -> s
```

Adds inner (dot) products.
Inner products should be linear and positive definite,
i.e. they should satisfy

- Linearity1: `(a *^ u) <.> v = a * (u <.> v)`
- Linearity2: `(u ^+^ v) <.> w = (u <.> w) + (v <.> w)`
- Positive Definite: `v <.> v = zero` iff `v = zeroV`


#### `(^/)`

``` purescript
(^/) :: forall v s. (VectorSpace v s, ModuloSemiring s) => v -> s -> v
```

Vector divided by scalar

#### `(^*)`

``` purescript
(^*) :: forall v s. (VectorSpace v s) => v -> s -> v
```

Vector multiplied by scalar

#### `lerp`

``` purescript
lerp :: forall v s. (AdditiveGroup v, VectorSpace v s) => v -> v -> s -> v
```

#### `magnitudeSq`

``` purescript
magnitudeSq :: forall v s. (InnerSpace v s) => v -> s
```

Square of the length of a vector.  Sometimes useful for efficiency.
See also 'magnitude'.

#### `magnitude`

``` purescript
magnitude :: forall v. (InnerSpace v Number) => v -> Number
```

Length of a vector.   See also 'magnitudeSq'.

#### `normalized`

``` purescript
normalized :: forall v s. (InnerSpace v Number) => v -> v
```

Vector in same direction as given one but with length of one.  If
given the zero vector, then return it.

#### `project`

``` purescript
project :: forall v. (InnerSpace v Number) => v -> v -> v
```

#### `vectorSpaceRing`

``` purescript
instance vectorSpaceRing :: (Ring s) => VectorSpace s s
```


#### `innerSpaceRing`

``` purescript
instance innerSpaceRing :: (Ring s) => InnerSpace s s
```


#### `vectorSpaceTuple`

``` purescript
instance vectorSpaceTuple :: (Ring s, VectorSpace u s, VectorSpace v s, AdditiveGroup (Tuple u v)) => VectorSpace (Tuple u v) s
```

#### `innerSpaceTuple`

``` purescript
instance innerSpaceTuple :: (Ring s, InnerSpace v s, AdditiveGroup (Tuple v v)) => InnerSpace (Tuple v v) s
```


## Module Data.VectorSpace.Vector

#### `vectorSpaceVec`

``` purescript
instance vectorSpaceVec :: (AdditiveGroup (V.Vec s v), Ring v) => VectorSpace (V.Vec s v) v
```

#### `innerSpaceVec`

``` purescript
instance innerSpaceVec :: (VectorSpace (V.Vec s v) v, Ring v) => InnerSpace (V.Vec s v) v
```


## Module Data.VectorSpace.Vector2

#### `additiveGroupVec2`

``` purescript
instance additiveGroupVec2 :: (V.Vector (V.Vec Two v), AdditiveGroup v) => AdditiveGroup (V.Vec Two v)
```


## Module Data.VectorSpace.Vector3

#### `additiveGroupVec3`

``` purescript
instance additiveGroupVec3 :: (V.Vector (V.Vec Three v), AdditiveGroup v) => AdditiveGroup (V.Vec Three v)
```


## Module Data.VectorSpace.Vector4

#### `additiveGroupVec4`

``` purescript
instance additiveGroupVec4 :: (V.Vector (V.Vec Four v), AdditiveGroup v) => AdditiveGroup (V.Vec Four v)
```



