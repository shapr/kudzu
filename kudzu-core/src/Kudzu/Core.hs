module Kudzu.Core (KudzuResult (..), examineTix, grabUntilNSame, tixModuleCount) where

import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..))

data KudzuResult a = KFail Int | KSuccess Int a deriving (Show, Eq, Ord)

-- | Keep running property tests until the "amount" of code coverage is the same for N iterations of one test.
grabUntilNSame ::
    (Monad m, Eq a) =>
    -- | How many iterations must be the same?
    Int ->
    -- | a lazy list of iterations
    [m a] ->
    m (KudzuResult a)
grabUntilNSame _ [] = pure $ KFail 0
grabUntilNSame orig (a : as) = do
    a' <- a -- run the first iteration of the test
    go 0 orig as a'
  where
    go c 0 _ z = pure $ KSuccess c z -- we reached the desired window size
    go c _ [] _ = pure $ KFail c -- if we run out of list elements for test results, we're done
    go c n (b : bs) z = do
        a' <- b
        if a' == z
            then go (c + 1) (n - 1) bs z
            else go (c + 1) orig as a'

-- | How many regions were executed at least once for this module?
tixCount :: TixModule -> Integer
tixCount (TixModule _ _ _ regions) = sum $ 1 <$ filter (> 0) regions

-- | How many regions were executed at least once for all these modules?
tixModuleCount :: Tix -> Integer
tixModuleCount (Tix ms) = sum $ map tixCount ms
