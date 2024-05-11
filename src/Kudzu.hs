{-# LANGUAGE OverloadedStrings #-}

module Kudzu where

import Control.Monad (unless)
import qualified Hedgehog as HH
import qualified Test.LeanCheck as LC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Random as QC
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..))

testUntilSameQCMany :: (Traversable t, QC.Testable a) => Int -> t a -> IO (t (KudzuResult Integer))
testUntilSameQCMany howMany ts = do
    mapM (testUntilSameQC howMany) ts

-- | QuickCheck
testUntilSameQC :: (QC.Testable a) => Int -> a -> IO (KudzuResult Integer)
testUntilSameQC n testable = do
    let rs = map (examineAndCount' testable) [0 .. n]
    grabUntilNSame n rs

examineAndCount' :: (QC.Testable prop) => prop -> Int -> IO Integer
examineAndCount' v size = do
    qcg <- QC.newQCGen
    QC.quickCheckWith (QC.stdArgs{QC.replay = Just (qcg, size)}) (QC.withMaxSuccess 1 v)
    tixModuleCount <$> examineTix

-- | Hedgehog
testUntilSameHHMany :: (Traversable t) => Int -> t HH.Property -> IO (t (KudzuResult Integer))
testUntilSameHHMany howMany ps = do
    mapM (testUntilSameHH howMany) ps

testUntilSameHH :: Int -> HH.Property -> IO (KudzuResult Integer)
testUntilSameHH n prop = grabUntilNSame n $ examineAndCountHH <$> repeat prop

examineAndCountHH :: HH.Property -> IO Integer
examineAndCountHH prop = do
    passed <- HH.check . HH.withTests 1 $ prop
    unless passed $ error "property failed"
    tixModuleCount <$> examineTix

-- | LeanCheck
testUntilSameLCMany :: (Traversable t, LC.Testable a) => Int -> t a -> IO (t (KudzuResult Integer))
testUntilSameLCMany howMany ts = do
    mapM (testUntilSameLC howMany) ts

testUntilSameLC :: (LC.Testable a) => Int -> a -> IO (KudzuResult Integer)
testUntilSameLC n testable = grabUntilNSame n $ examineAndCount <$> LC.results testable

examineAndCount :: ([String], Bool) -> IO Integer
examineAndCount v = unless (snd v) (error $ unwords ("test failed with:" : fst v)) >> tixModuleCount <$> examineTix

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
