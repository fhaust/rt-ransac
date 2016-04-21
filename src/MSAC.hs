{-# LANGUAGE FlexibleContexts #-}

module MSAC
    ( msac
    , msacM
    , module Ransac.Common
    ) where


import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector         as BV

import           Data.Function
import           Data.Bifunctor
import           Data.List

import           Control.Monad
import           Control.Monad.Random

import           Linear hiding (trace)

import           Util (vFilterFoldBy)

import           Debug.Trace

import           Ransac.Common


type Iterations  = Int
type SampleCount = Int
type MinInliers  = Int


msac :: (MonadRandom m, V.Vector v a)
       => Iterations              -- ^ iterations to run
       -> SampleCount             -- ^ number of datapoints to sample for initial fitting
       -> (v a -> Maybe model)    -- ^ a fitting function producing a model from the datapoints
       -> (model -> Bool)
       -> (model -> a -> Double)  -- ^ an error function to calculate the error of a datapoint to the model
       -> (Double -> Bool)        -- ^ a function to check if a point is an inlier based on the error function
       -> v a                     -- ^ the datapoints
       -> m (BV.Vector (Maybe (Result model (v a) Double)))
msac iterations sampleCount fit = msacM iterations sampleCount (return . fit)

{-# INLINE msac #-}


msacM :: (MonadRandom m, V.Vector v a)
        => Iterations              -- ^ iterations to run
        -> SampleCount             -- ^ number of datapoints to sample for initial fitting
        -> (v a -> m (Maybe model))    -- ^ a fitting function producing a model from the datapoints
        -> (model -> Bool)
        -> (model -> a -> Double)  -- ^ an error function to calculate the error of a datapoint to the model
        -> (Double -> Bool)        -- ^ a function to check if a point is an inlier based on the error function
        -> v a                     -- ^ the datapoints
        -> m (BV.Vector (Maybe (Result model (v a) Double)))
msacM iterations sampleCount fit check distance isInlier v = BV.replicateM iterations go
  where go = do
          samples <- drawSamples sampleCount v
          maybeModel <- fit samples

          case maybeModel of
            Nothing      -> return Nothing
            (Just model) ->
              if check model 
                then do
                  let (inliers,error) = vFilterFoldBy (distance model) (+) 0 isInlier v -- too many constraints :(
                  return $ Just (Result model inliers error)
                else 
                  return Nothing



{-# INLINE msacM #-}
