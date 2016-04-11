

module LineFitting where

import qualified Data.Vector.Unboxed as V

import           Linear

import           Control.Monad.Random


-- TODO ... clean this up
fitLine :: V.Vector (V2 Double) -> (Double,Double)
fitLine vs = (α,β)
    where (mx,my) = V.foldl' (\(mx,my) (V2 x y) -> (mx+x,my+y)) (0,0) vs
          mx'   = mx / c
          my'   = my / c

          (n,d) = V.foldl' (\(n,d) (V2 x y) -> (n + (x-mx')*(y-my'), d + (x-mx')^2)) (0,0) vs
          β     = n / d
          α     = my' - β * mx'
          c     = fromIntegral $ V.length vs


randomLine :: MonadRandom m => m (V.Vector (V2 Double))
randomLine = do
    α <- getRandom
    β <- getRandom

    V.replicateM 100 (randomPointOnLine α β)

randomPointOnLine :: MonadRandom m => Double -> Double -> m (V2 Double)
randomPointOnLine α β = do
    dx <- getRandomR (-0.01,0.01)
    dy <- getRandomR (-0.01,0.01)

    x  <- getRandom

    let y = α + β * x

    return $ V2 (x+dx) (y+dy)

distanceToLine :: (Double, Double) -> V2 Double -> Double
distanceToLine (α,β) (V2 x y) = abs (y - (α + x * β))

randomFittingProblem :: Int -> Int -> Double -> IO (Double,Double,V.Vector (V2 Double))
randomFittingProblem numInliers numOutliers noise = do
    -- setup a generic fitting problem
    α <- getRandomR (-1,1) 
    β <- getRandomR (-10,10)

    -- sample random points on line
    ps <- V.replicateM numInliers (randomPointOnLine α β)

    -- sample random points as noise
    ns <- V.replicateM numOutliers (V2 <$> getRandomR (-noise,noise) <*> getRandomR (-noise,noise))

    -- test set
    let points = V.concat [ps,ns]

    return (α,β,points)
