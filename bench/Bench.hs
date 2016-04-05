module Main where

import qualified Data.Vector.Unboxed as U

import Criterion.Main

import LineFitting
import Linear

import qualified Numeric.Ransac as ACow
import qualified Lib            as NG

import           Control.Monad.Random

main = do


    -- setup a generic fitting problem
    let α = 0
        β = 1

    -- sample random points
    ps <- U.replicateM 100 (randomPointOnLine α β)

    -- generate some noise

    defaultMain 
      [ bench "ransac (acow)" $ nfIO $ ACow.ransac 3 2 0.9 (Just . fitLine) distanceToLine (< 0.01) ps
      , bench "ransac-ng"     $ nfIO $ NG.ransacIO 3 2 90 fitLine (\l p -> distanceToLine l p < 0.01) ps
      ]
