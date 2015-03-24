module Data.VectorSpace.Vector where

import qualified Data.Vector as V
import Data.AdditiveGroup
import Data.VectorSpace
import Data.Foldable (foldl)


--instance vectorSpaceVec2 :: (VectorSpace v v) => VectorSpace (V.Vec Two v) v where
instance vectorSpaceVec :: (AdditiveGroup (V.Vec s v), AdditiveGroup v, VectorSpace v v) => VectorSpace (V.Vec s v) v where
  (*^) s v = (s *^) <$> v

-- instance innerSpaceVec2 :: (InnerSpace v v) => InnerSpace (V.Vec Two v) v where
instance innerSpaceVec :: (VectorSpace (V.Vec s v) v, AdditiveGroup v, InnerSpace v v) => InnerSpace (V.Vec s v) v where
  (<.>) v v' = foldl (^+^) zeroV $ (<.>) <$> v <*> v'