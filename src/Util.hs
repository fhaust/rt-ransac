

module Util where


import qualified Data.Vector.Generic as G
import           Data.STRef

import           Control.Monad.ST



vFilterFold :: G.Vector v b => (a -> b -> a) -> a -> (b -> Bool) -> v b -> (v b, a)
vFilterFold = vFilterFoldBy id

{-# INLINE vFilterFold #-}

vFilterFoldBy :: G.Vector v a => (a -> b) -> (c -> b -> c) -> c -> (b -> Bool) -> v a -> (v a, c)
vFilterFoldBy by f i p vs = runST $ do
    acc <- newSTRef i
    let go b = do
            let b' = by b
            if p b' then modifySTRef' acc (`f` b') >> return True
                    else return False

    f <- G.filterM go vs
    r <- readSTRef acc
    return (f, r)

{-# INLINE vFilterFoldBy #-}
