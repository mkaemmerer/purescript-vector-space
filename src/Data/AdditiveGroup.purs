module Data.AdditiveGroup where

import Prelude

import Control.Apply
import Data.Tuple
import Data.Foldable

infixl 6 ^+^
infixl 6 ^-^


-- | Additive groups should satisfy these laws
-- |
-- | - Associativity: `x ^+^ (y ^+^ z) = (x ^+^ y) ^+^ z`
-- | - Commutativity: `x ^+^ y = y ^+^ x`
-- | - Right Identity: `x ^+^ zeroV = x`
-- | - Left Identity: `zeroV ^+^ x = x`
-- | - Inverse: `x ^+^ negateV x = negateV x ^+^ x = zeroV`
-- |
class AdditiveGroup v where
  -- | The zero element: identity for '(^+^)'
  zeroV :: v
  -- | Add vectors
  (^+^) :: v -> v -> v
  -- | Additive inverse
  negateV :: v -> v

-- | Group subtraction
(^-^) :: forall v. (AdditiveGroup v) => v -> v -> v
(^-^) v v' = v ^+^ negateV v'

-- | Sum over several vectors
sumV :: forall f v. (Foldable f, AdditiveGroup v) => f v -> v
sumV = foldr (^+^) zeroV



instance additiveGroupUnit :: AdditiveGroup Unit where
  zeroV     = unit
  (^+^) _ _ = unit
  negateV   = id

instance additiveGroupRing :: (Ring s) => AdditiveGroup s where
  zeroV     = zero
  (^+^) x y = x + y
  negateV x = negate x

instance additiveGroupTuple :: (AdditiveGroup u, AdditiveGroup v) => AdditiveGroup (Tuple u v) where
  zeroV                           = Tuple zeroV zeroV
  (^+^) (Tuple u v) (Tuple u' v') = Tuple (u^+^u') (v^+^v')
  negateV (Tuple u v)             = Tuple (negateV u) (negateV v)

instance additiveGroupArr :: (AdditiveGroup v) => AdditiveGroup (u -> v) where
  zeroV   = pure zeroV
  (^+^)   = lift2 (^+^)
  negateV = (negateV <$>)

