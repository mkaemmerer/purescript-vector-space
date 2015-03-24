module Data.VectorSpace.Vector4 where

import Data.TypeNat
import qualified Data.Vector as V
import Data.AdditiveGroup
import Control.Apply (lift2)


instance additiveGroupVec4 :: (V.Vector (V.Vec Four v), AdditiveGroup v) => AdditiveGroup (V.Vec Four v) where
  zeroV     = V.fromArray [zeroV, zeroV, zeroV, zeroV]
  (^+^)     = lift2 (^+^)
  negateV v = negateV <$> v