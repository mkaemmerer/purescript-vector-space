module Data.VectorSpace.Vector2 where

import Data.TypeNat
import qualified Data.Vector as V
import Data.AdditiveGroup
import Control.Apply (lift2)


instance additiveGroupVec2 :: (V.Vector (V.Vec Two v), AdditiveGroup v) => AdditiveGroup (V.Vec Two v) where
  zeroV     = V.fromArray [zeroV, zeroV]
  (^+^)     = lift2 (^+^)
  negateV v = negateV <$> v