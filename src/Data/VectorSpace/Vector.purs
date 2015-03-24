module Data.VectorSpace.Vector where

import qualified Data.Vector as V
import Data.AdditiveGroup
import Data.VectorSpace
import Data.Foldable (foldl)


instance vectorSpaceVec :: (AdditiveGroup (V.Vec s v), Ring v) => VectorSpace (V.Vec s v) v where
  (*^) s v = (s *^) <$> v

-- instance innerSpaceVec :: (VectorSpace (V.Vec s v) v) => InnerSpace (V.Vec s v) v where
instance innerSpaceVec :: (VectorSpace (V.Vec s v) v, Ring v) => InnerSpace (V.Vec s v) v where
  (<.>) v v' = foldl (^+^) zeroV $ (<.>) <$> v <*> v'