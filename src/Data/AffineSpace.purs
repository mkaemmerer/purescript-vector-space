module Data.AffineSpace where

import Data.Tuple
import Data.AdditiveGroup
import Data.VectorSpace

infixr 6 .-.
infixl 6 .+^

-- | Affine spaces
class (VectorSpace v s) <= AffineSpace p v s where
  -- | Subtract points
  (.-.) :: p -> p -> v
  -- | Add a vector to a point
  (.+^) :: p -> v -> p


-- | Affine interpolation between `a` (when `t==0`) and `b` (when `t==1`).
alerp :: forall p v s. (AffineSpace p v s) => p -> p -> s -> p
alerp a b t = a .+^ (t *^ (a .-. b))

-- | Square of the distance between two points. Useful for efficiency.
-- | See also 'distance'.
distanceSq :: forall p v s. (AffineSpace p v s, InnerSpace v s) => p -> p -> s
distanceSq p p' = magnitudeSq $ p .-. p'

-- | Distance between two points. See also 'distanceSq'.
distance :: forall p v. (AffineSpace p v Number, InnerSpace v Number) =>  p -> p -> Number
distance p p' = magnitude $ p .-. p'


instance affineSpaceRing :: (Ring s) => AffineSpace s s s where
  (.-.) x y = x - y
  (.+^) p v = p + v

instance affineSpaceTuple :: (AffineSpace p u s, AffineSpace q v s) => AffineSpace (Tuple p q) (Tuple u v) s where
  (.-.) (Tuple p q) (Tuple p' q') = Tuple (p - p') (q - q')
  (.+^) (Tuple p q) (Tuple u v)   = Tuple (p .+^ u) (q .+^ v)