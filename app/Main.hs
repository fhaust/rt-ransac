module Main where

import Lib


import qualified Data.Vector.Unboxed as V
import qualified Data.Vector         as BV

import           Control.Monad.Random

import           Linear
import           LineFitting

main :: IO ()
main = putStrLn "stuff"




{-runFitLine :: IO (BV.Vector (Double, Double))-}
{-runFitLine = do-}
{-    α <- getRandom-}
{-    β <- getRandom-}

{-    putStrLn $ "line: " ++ show (α,β)-}

{-    linePoints <- V.replicateM 100 (randomPointOnLine α β)-}

{-    noise <- V.replicateM 10 (V2 <$> getRandom <*> getRandom)-}

{-    let dataPoints = V.concat [linePoints, noise]-}

{-    ransac 100 2 fitLine (\l p -> distanceToLine l p < 0.01) dataPoints -}
