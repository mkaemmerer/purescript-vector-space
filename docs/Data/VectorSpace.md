## Module Data.VectorSpace

#### `VectorSpace`

``` purescript
class (Ring s, AdditiveGroup v) <= VectorSpace v s where
  scaleV :: s -> v -> v
```

Vector spaces should satisfy these laws

- Identity: `one *^ v = v`
- Compatibility: `a * (b *^ v) = (a * b) *^ v`
- Distributivity1: `a *^ (u ^+^ v) = (a *^ u) ^+^ (a *^ v)`
- Distributivity2: `(a + b) *^ v = (a *^ v) + (b *^ v)`


##### Instances
``` purescript
instance vectorSpaceRing :: (Ring s) => VectorSpace s s
instance vectorSpaceTuple :: (Ring s, AdditiveGroup (Tuple u v), VectorSpace u s, VectorSpace v s) => VectorSpace (Tuple u v) s
instance vectorSpaceArr :: (Ring s, AdditiveGroup (a -> v), VectorSpace v s) => VectorSpace (a -> v) s
instance vectorSpaceArr2 :: (Ring (a -> s), AdditiveGroup (a -> v), VectorSpace v s) => VectorSpace (a -> v) (a -> s)
instance vectorSpaceVec :: (AdditiveGroup (Vec s v), Ring v) => VectorSpace (Vec s v) v
```

#### `(*^)`

``` purescript
(*^) :: forall v s. (VectorSpace v s) => s -> v -> v
```

_right-associative / precedence 7_

Alias for `scaleV`

#### `InnerSpace`

``` purescript
class (VectorSpace v s) <= InnerSpace v s where
  innerProduct :: v -> v -> s
```

Adds inner (dot) products.
Inner products should be linear and positive definite,
i.e. they should satisfy

- Linearity1: `(a *^ u) <.> v = a * (u <.> v)`
- Linearity2: `(u ^+^ v) <.> w = (u <.> w) + (v <.> w)`
- Positive Definite: `v <.> v = zero` iff `v = zeroV`


##### Instances
``` purescript
instance innerSpaceRing :: (Ring s) => InnerSpace s s
instance innerSpaceTuple :: (Ring s, AdditiveGroup (Tuple v v), InnerSpace v s) => InnerSpace (Tuple v v) s
instance innerSpaceArr :: (VectorSpace (a -> v) (a -> s), InnerSpace v s) => InnerSpace (a -> v) (a -> s)
instance innerSpaceVec :: (VectorSpace (Vec s v) v, Ring v) => InnerSpace (Vec s v) v
```

#### `(<.>)`

``` purescript
(<.>) :: forall v s. (InnerSpace v s) => v -> v -> s
```

_right-associative / precedence 7_

Alias for `innerProduct`

#### `(^/)`

``` purescript
(^/) :: forall v s. (VectorSpace v s, ModuloSemiring s) => v -> s -> v
```

_right-associative / precedence 7_

Vector divided by scalar

#### `(^*)`

``` purescript
(^*) :: forall v s. (VectorSpace v s) => v -> s -> v
```

_right-associative / precedence 7_

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


