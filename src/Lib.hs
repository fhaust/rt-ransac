module Lib
    ( drawSamples
    , ransac
    , ransacIO
    ) where


import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as BV

import           Data.Function
import           Data.List

import           Control.Monad.Random

import Linear hiding (trace)

import Debug.Trace

-- | draw `n` random samples from `v`
-- note that this may produce the same sample twice and we don't safeguard
-- against that
drawSamples :: (MonadRandom m, V.Vector v a) => Int -> v a -> m (v a)
drawSamples n v = do
    let l = V.length v - 1
    is <- nub <$> getRandomRs (0,l)
    return $ V.fromListN n . map (V.unsafeIndex v) $ is

type Iterations  = Int
type SampleCount = Int
type MinInliers  = Int

{-# INLINE drawSamples #-}

{-drawIntialSamples :: Int -> v a -> m (-}

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
