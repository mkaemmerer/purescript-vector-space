module Data.VectorSpace where

import Prelude

import Math
import Control.Apply
import Data.Tuple
import Data.AdditiveGroup
import Data.Vector
import Data.Foldable (foldl)

infixr 7 *^
infixr 7 <.>
infixr 7 ^/
infixr 7 ^*


-- | Vector spaces should satisfy these laws
-- | 
-- | - Identity: `one *^ v = v`
-- | - Compatibility: `a * (b *^ v) = (a * b) *^ v`
-- | - Distributivity1: `a *^ (u ^+^ v) = (a *^ u) ^+^ (a *^ v)`
-- | - Distributivity2: `(a + b) *^ v = (a *^ v) + (b *^ v)`
-- |
class (Ring s, AdditiveGroup v) <= VectorSpace v s where
  -- | Scale a vector
  (*^) :: s -> v -> v

-- | Adds inner (dot) products.
-- | Inner products should be linear and positive definite,
-- | i.e. they should satisfy
-- | 
-- | - Linearity1: `(a *^ u) <.> v = a * (u <.> v)`
-- | - Linearity2: `(u ^+^ v) <.> w = (u <.> w) + (v <.> w)`
-- | - Positive Definite: `v <.> v = zero` iff `v = zeroV`
-- |
class (VectorSpace v s) <= InnerSpace v s where
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
instance vectorSpaceTuple :: (Ring s, AdditiveGroup (Tuple u v), VectorSpace u s, VectorSpace v s) => VectorSpace (Tuple u v) s where
  (*^) s (Tuple u v) = Tuple (s*^u) (s*^v)

-- TODO: the constraints (AdditiveGroup (Tuple v v)) and (AdditiveGroup s) should be inferred from (InnerSpace v s)
-- instance innerSpaceTuple :: (InnerSpace v s) => InnerSpace (Tuple v v) s where
instance innerSpaceTuple :: (Ring s, AdditiveGroup (Tuple v v), InnerSpace v s) => InnerSpace (Tuple v v) s where
  (<.>) (Tuple u v) (Tuple u' v') = (u <.> u') ^+^ (v <.> v')


instance vectorSpaceArr :: (Ring s, AdditiveGroup (a -> v), VectorSpace v s) => VectorSpace (a -> v) s where
  (*^) s = (<$>) (s *^)

instance vectorSpaceArr2 :: (Ring (a -> s), AdditiveGroup (a -> v), VectorSpace v s) => VectorSpace (a -> v) (a -> s) where
  (*^) = lift2 (*^)

instance innerSpaceArr :: (VectorSpace (a -> v) (a -> s), InnerSpace v s) => InnerSpace (a -> v) (a -> s) where
 (<.>) = lift2 (<.>)


instance vectorSpaceVec :: (AdditiveGroup (Vec s v), Ring v) => VectorSpace (Vec s v) v where
  (*^) s v = (s *^) <$> v

-- instance innerSpaceVec :: (VectorSpace (Vec s v) v) => InnerSpace (Vec s v) v where
instance innerSpaceVec :: (VectorSpace (Vec s v) v, Ring v) => InnerSpace (Vec s v) v where
  (<.>) v v' = foldl (^+^) zeroV $ (<.>) <$> v <*> v'
