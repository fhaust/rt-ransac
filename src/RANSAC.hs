module RANSAC
    ( drawSamples
    , ransac
    , ransacIO
    ) where


import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as BV

import           Data.Function

import           Control.Monad.Random

import           Linear hiding (trace)

import           Debug.Trace

import           Ransac.Common


type Iterations  = Int
type SampleCount = Int
type MinInliers  = Int


ransac :: (MonadRandom m, V.Vector v a)
       => Iterations -> SampleCount -> MinInliers -> (v a -> model) -> (model -> a -> Bool) -> v a -> m (Maybe (model, v a))
ransac iterations sampleCount minInliers fit goodFit v = check . BV.maximumBy (compare `on` numInliers) <$> BV.replicateM iterations go
  where go = do
          samples <- drawSamples sampleCount v
          let model   = fit samples
              inliers = V.filter (goodFit model) v
          return (fit inliers, inliers)

        {-success (_,inliers) = V.length inliers > minInliers-}
        numInliers (_,inliers) = V.length inliers
        check result@(_,inliers) = if V.length inliers > minInliers then Just result else Nothing

{-# INLINE ransac #-}

ransacIO :: V.Vector v a 
         => Iterations -> SampleCount -> MinInliers -> (v a -> model) -> (model -> a -> Bool) -> v a 
         -> IO (Maybe (model, v a))
ransacIO is sc mi fit gf vs = evalRandIO $ ransac is sc mi fit gf vs

{-# INLINE ransacIO #-}
