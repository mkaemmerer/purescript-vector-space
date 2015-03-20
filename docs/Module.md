# Module Documentation

## Module Data.AdditiveGroup

#### `AdditiveGroup`

``` purescript
class AdditiveGroup v where
  zeroV :: v
  (^+^) :: v -> v -> v
  negateV :: v -> v
```

AdditiveGroups should satisfy the following laws

- Associativity: `x ^+^ (y ^+^ z) = (x ^+^ y) ^+^ z`
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
class (AdditiveGroup v) <= VectorSpace v s where
  (*^) :: s -> v -> v
```

Vector space `v`.

#### `InnerSpace`

``` purescript
class (VectorSpace v s, AdditiveGroup s) <= InnerSpace v s where
  (<.>) :: v -> v -> s
```

Adds inner (dot) products.

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
instance vectorSpaceTuple :: (VectorSpace u s, VectorSpace v s, AdditiveGroup (Tuple u v)) => VectorSpace (Tuple u v) s
```

#### `innerSpaceTuple`

``` purescript
instance innerSpaceTuple :: (InnerSpace v s, AdditiveGroup (Tuple v v), AdditiveGroup s) => InnerSpace (Tuple v v) s
```