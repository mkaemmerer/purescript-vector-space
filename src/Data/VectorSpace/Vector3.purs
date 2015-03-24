module Data.VectorSpace.Vector3 where

import Data.TypeNat
import qualified Data.Vector as V
import Data.AdditiveGroup
import Control.Apply (lift2)


-- instance additiveGroupVec3 :: (AdditiveGroup v) => AdditiveGroup (V.Vec Three v) where
instance additiveGroupVec3 :: (V.Vector (V.Vec Three v), AdditiveGroup v) => AdditiveGroup (V.Vec Three v) where
  zeroV     = V.fromArray [zeroV, zeroV, zeroV]
  (^+^)     = lift2 (^+^)
  negateV v = negateV <$> v