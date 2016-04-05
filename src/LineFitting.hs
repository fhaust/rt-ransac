

module LineFitting where

import qualified Data.Vector.Unboxed as V

import           Linear

import           Control.Monad.Random

fitLine :: V.Vector (V2 Double) -> (Double,Double)
fitLine vs = (α,β)
    where (xy,x,y,x2) = V.foldl' (\(xy,x,y,x2) (V2 a b) -> (xy+a*b,x+a,y+b,x2+a*a)) (0,0,0,0) vs
          β           = (xy - x*y) / (x2-x^2)
          α           = y - β * x



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
