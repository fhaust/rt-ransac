
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.Vector.Unboxed as U

import Criterion.Main

import LineFitting
import Linear

import qualified Numeric.Ransac as ACow
import qualified Lib            as NG
import qualified MSAC

import           Control.Monad.Random
import           Control.DeepSeq
import           Control.Exception (evaluate)

main = do

    -- test set
    (α,β,force -> ps) <- randomFittingProblem 100 100 0.01

    -- generate some noise

    defaultMain
      [ bench "ransac (acow)" $ nfIO $ ACow.ransac 3 2 0.2 (Just . fitLine) distanceToLine (< 0.01) ps
      , bench "ransac-ng"     $ nfIO $ NG.ransacIO 3 2 20 fitLine (\l p -> distanceToLine l p < 0.01) ps
      , bench "msac"          $ nfIO $ MSAC.msacIO (const True) 3 2 fitLine distanceToLine (< 0.01) ps
      ]
