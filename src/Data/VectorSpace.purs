module Data.VectorSpace where

import Math
import Control.Apply
import Data.Tuple
import Data.AdditiveGroup

infixr 7 *^
infixr 7 <.>
infixr 7 ^/
infixr 7 ^*


-- | Vector space `v`.
class (AdditiveGroup v) <= VectorSpace v s where
  -- | Scale a vector
  (*^) :: s -> v -> v

-- | Adds inner (dot) products.
class (VectorSpace v s, AdditiveGroup s) <= InnerSpace v s where
  -- | Inner/dot product
  (<.>) :: v -> v -> s

-- | Vector divided by scalar
(^/) :: forall v s. (VectorSpace v s, ModuloSemiring s) => v -> s -> v
(^/) v s = (one/s) *^ v

-- | Vector multiplied by scalar
(^*) :: forall v s. (VectorSpace v s) => v -> s -> v
(^*) = flip (*^)

-- | Linear interpolation between `a` (when `t==0`) and `b` (when `t==1`).
-- TODO: the constraint (AdditiveGroup v) should be inferred from (VectorSpace v s)
-- lerp :: forall v s. (VectorSpace v s) => v -> v -> s -> v
lerp :: forall v s. (AdditiveGroup v, VectorSpace v s) => v -> v -> s -> v
lerp a b t = a ^+^ t *^ (b ^-^ a)

-- | Square of the length of a vector.  Sometimes useful for efficiency.
-- | See also 'magnitude'.
magnitudeSq :: forall v s. (InnerSpace v s) => v -> s
magnitudeSq v = v <.> v

-- | Length of a vector.   See also 'magnitudeSq'.
magnitude :: forall v. (InnerSpace v Number) =>  v -> Number
magnitude = sqrt <<< magnitudeSq

-- | Vector in same direction as given one but with length of one.  If
-- | given the zero vector, then return it.
normalized :: forall v s. (InnerSpace v Number) =>  v -> v
normalized v = v ^/ magnitude v

-- | `project u v` computes the projection of `v` onto `u`.
-- TODO: should be possible to make this more general than `InnerSpace v Number`
-- project :: forall v s. (InnerSpace v s, ModuloSemiring s) => v -> v -> v
project :: forall v. (InnerSpace v Number) => v -> v -> v
project u v = ((v <.> u) / magnitudeSq u)::Number *^ u



instance vectorSpaceRing :: (Ring s) => VectorSpace s s where
  (*^) x y = x * y

instance innerSpaceRing :: (Ring s) => InnerSpace s s where
  (<.>) x y = x * y


-- TODO: the constraint (AdditiveGroup (Tuple u v)) should be inferred from (VectorSpace u s) and (VectorSpace v s)
-- instance vectorSpaceTuple :: (VectorSpace u s, VectorSpace v s) => VectorSpace (Tuple u v) s where
instance vectorSpaceTuple :: (VectorSpace u s, VectorSpace v s, AdditiveGroup (Tuple u v)) => VectorSpace (Tuple u v) s where
  (*^) s (Tuple u v) = Tuple (s*^u) (s*^v)

-- TODO: the constraints (AdditiveGroup (Tuple v v)) and (AdditiveGroup s) should be inferred from (InnerSpace v s)
-- instance innerSpaceTuple :: (InnerSpace v s) => InnerSpace (Tuple v v) s where
instance innerSpaceTuple :: (InnerSpace v s, AdditiveGroup (Tuple v v), AdditiveGroup s) => InnerSpace (Tuple v v) s where
  (<.>) (Tuple u v) (Tuple u' v') = (u <.> u') ^+^ (v <.> v')
