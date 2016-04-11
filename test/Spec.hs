

import           Data.List
import qualified Data.Vector.Unboxed as U

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.QuickCheck.Monadic


import qualified Numeric.Ransac as ACow
import qualified RANSAC
import qualified MSAC

import           LineFitting

import           Linear

import           Debug.Trace

main :: IO ()
main = defaultMain $ testGroup "Test Suite"
  [ groupBasics
  , groupNoisy
  ]


-- check that the algorithms actually are able to fit lines

propACowIsRight (LFP (α,β,ps)) = monadicIO $ do
  acow <- run $ ACow.ransac 3 2 0.9 (Just . fitLine) distanceToLine (< 0.1) ps
  assert $ case acow of
    Just ((α',β'),_) -> abs (α - α') < 1e-9 && abs (β - β') < 1e-9
    Nothing          -> False

propNGIsRight (LFP (α,β,ps)) = monadicIO $ do
  ng <- run $ RANSAC.ransacIO 3 2 90 fitLine (\l p -> distanceToLine l p < 0.1) ps
  assert $ case ng of
    Just ((α',β'),_) -> abs (α - α') < 1e-9 && abs (β - β') < 1e-9
    Nothing          -> False

propMSACIsRight (LFP (α,β,ps)) = monadicIO $ do
  result <- run $ MSAC.msacIO (const True) 3 2 fitLine distanceToLine (< 0.1) ps
  assert $ case result of
    Just ((α',β'),_,_) -> traceShow (α',β') $ abs (α - α') < 1e-9 && abs (β - β') < 1e-9
    Nothing            -> False

propFitLineWorks (LFP (α,β,ps)) = let (α',β') = fitLine ps in α `epEq` α' && β `epEq` β'
    where epEq a b = abs (a - b) < 1e-6

groupBasics = testGroup "basic properties"
  [ testProperty "fitline" propFitLineWorks
  , testProperty "acow" propACowIsRight
  , testProperty "ng" propNGIsRight
  , testProperty "msac" propMSACIsRight
  ]



newtype LineFittingProblem = LFP { unLFP :: (Double,Double,U.Vector (V2 Double)) } deriving (Show, Read, Eq)

instance Arbitrary LineFittingProblem where
    arbitrary = do
      -- get line parameters
      α <- arbitrary
      β <- arbitrary

      protoPs <- vectorOf 100 arbitrary `suchThat` notAllTheSame
      let ps = U.fromList . map (\x -> V2 x (α + x * β)) $ protoPs

      return $ LFP (α,β,ps)


notAllTheSame ps = length (nub ps) > 1


-- check that the algorithms can handle noise


propACowNoisy (NLFP α β δ ps) = monadicIO $ do
  acow <- run $ ACow.ransac 100 2 0.5 (Just . fitLine) distanceToLine (< 0.1) ps
  assert $ case acow of
    Just ((α',β'),_) -> abs (α - α') < 1e-1 && abs (β - β') < 1e-1
    Nothing          -> False

propNGNoisy (NLFP α β δ ps) = monadicIO $ do
  ng <- run $ RANSAC.ransacIO 100 2 50 fitLine (\l p -> distanceToLine l p < 0.1) ps
  assert $ case ng of
    Just ((α',β'),_) -> abs (α - α') < 1e-1 && abs (β - β') < 1e-1
    Nothing          -> False

propNoisyACowEqNG (NLFP α β δ ps) = monadicIO $ do

  acow <- run $ ACow.ransac 100 2 0.5 (Just . fitLine) distanceToLine (< 0.1) ps
  ng   <- run $ RANSAC.ransacIO 100 2 50 fitLine (\l p -> distanceToLine l p < 0.1) ps

  assert $ case (acow,ng) of
    (Just ((α0,β0),_), Just ((α1,β1),_)) -> abs (α0 - α1) < 1e-1  && abs (β0 - β1) < 1e-1
    _                                    -> False

groupNoisy = testGroup "test on noisy data"
  {-[ testProperty "acow" propACowNoisy-}
  {-, testProperty "ng"   propNGNoisy-}
  [ testProperty "eq"   propNoisyACowEqNG
  ]

data NoisyLineFittingProblem = NLFP {
    nlfpAlpha :: Double,
    nlfpBeta  :: Double,
    nlfpDelta :: Double,
    nlfpPoints :: U.Vector (V2 Double)
} deriving (Show, Read, Eq)

instance Arbitrary NoisyLineFittingProblem where
    arbitrary = do
      -- get line parameters
      (LFP (α,β,inliers)) <- arbitrary

      -- sample noise
      δ <- choose (0,0.1)
      noise <- U.fromList <$> vectorOf (U.length inliers) (V2 <$> choose (-δ,δ) <*> choose (-δ,δ))

      -- gather data
      let output = U.zipWith (+) inliers noise

      return $ NLFP α β δ output


