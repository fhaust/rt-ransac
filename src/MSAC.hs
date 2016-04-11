{-# LANGUAGE FlexibleContexts #-}

module MSAC
    ( drawSamples
    , msac
    , msacIO
    ) where


import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as BV

import           Data.Function
import           Data.Bifunctor
import           Data.List

import           Control.Monad.Random

import           Linear hiding (trace)

import           Util (vFilterFoldBy)

import           Debug.Trace

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

msac :: (MonadRandom m, V.Vector v a, V.Vector v Double, V.Vector v (a,Double))
       => Iterations              -- ^ iterations to run
       -> SampleCount             -- ^ number of datapoints to sample for initial fitting
       -> (v a -> model)          -- ^ a fitting function producing a model from the datapoints
       -> (model -> a -> Double)  -- ^ an error function to calculate the error of a datapoint to the model
       -> (Double -> Bool)        -- ^ a function to check if a point is an inlier based on the error function
       -> v a                     -- ^ the datapoints
       -> m (BV.Vector (model, v a, Double))
msac iterations sampleCount fit distance isInlier v = BV.replicateM iterations go
  where go = do
          samples <- drawSamples sampleCount v
          let model = fit samples

          let (inliers,error) = vFilterFoldBy (distance model) (+) 0 isInlier v

          return (fit inliers, inliers, error)


{-# INLINE msac #-}

msacBest :: (V.Vector v a) => (Double -> Bool) -> BV.Vector (model, v a, Double) -> Maybe (model, v a, Double)
msacBest check vs = if check e then Just best else Nothing
    where best@(_,_,e) = V.minimumBy (\(_,_,a) (_,_,b) -> compare a b) vs

{-# INLINE msacBest #-}


msacIO
  :: (V.Vector v a, V.Vector v Double, V.Vector v (a, Double)) =>
     (Double -> Bool)
     -> Iterations
     -> SampleCount
     -> (v a -> model)
     -> (model -> a -> Double)
     -> (Double -> Bool)
     -> v a
     -> IO (Maybe (model, v a, Double))
msacIO check is sc fit dist ii ps = evalRandIO (msacBest check <$> msac is sc fit dist ii ps)

{-# INLINE msacIO #-}
