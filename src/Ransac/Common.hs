

module Ransac.Common where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

import           Data.Maybe
import           Data.List

import           Control.Monad.Random

type FittingFunction ps model = ps -> Maybe model


data Result m p c = Result { rModel :: m, rPoints :: p, rCost :: c } deriving (Show, Read, Eq)

rNumInliers :: G.Vector v a => Result m (v a) c -> Int
rNumInliers = G.length . rPoints

bestResult :: (G.Vector v a, Ord c) => V.Vector (Result m (v a) c) -> Maybe (Result m (v a) c)
bestResult = V.foldl' go Nothing
    where go Nothing  b = Just b
          go (Just a) b | rNumInliers a < rNumInliers b                        = Just a 
                        | rNumInliers a == rNumInliers b &&  rCost a < rCost b = Just a
                        | otherwise                                            = Just b

{-# INLINE bestResult #-}


onlyResults :: V.Vector (Maybe (Result m p c)) -> V.Vector (Result m p c)
onlyResults = V.map fromJust . V.filter isJust

{-# INLINE onlyResults #-}

runRansac = evalRandIO 


-- | draw `n` random samples from `v`
drawSamples :: (MonadRandom m, G.Vector v a) => Int -> v a -> m (v a)
drawSamples n v = do
    let l = G.length v - 1
    is <- nub <$> getRandomRs (0,l)
    return $ G.fromListN n . map (G.unsafeIndex v) $ is

{-# INLINE drawSamples #-}


-- | necessaryIterations p ε s = calculate number of iterations to achieve at
-- least a propability 'p' that a valid model for 's' points was draw
-- considering a ratio of 'ε' of inliers / outliers.
necessaryIterations :: (Floating r, Integral b, Integral b1, RealFrac r) => r -> r -> b1 -> b
necessaryIterations p ε s = ceiling $ logBase (1 - (1 - ε)^s) (1 - p) 
