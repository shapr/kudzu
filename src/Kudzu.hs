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
    r1 <- head rs
    grabUntilNSame 0 n n (tail rs) r1

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
testUntilSameHH n prop = do
    let rs = examineAndCountHH <$> repeat prop
    r1 <- head rs
    grabUntilNSame 0 n n (tail rs) r1

examineAndCountHH :: HH.Property -> IO Integer
examineAndCountHH prop = do
    passed <- HH.check prop
    unless passed $ error "property failed"
    tixModuleCount <$> examineTix

-- | LeanCheck
testUntilSameLCMany :: (Traversable t, LC.Testable a) => Int -> t a -> IO (t (KudzuResult Integer))
testUntilSameLCMany howMany ts = do
    mapM (testUntilSameLC howMany) ts

testUntilSameLC :: (LC.Testable a) => Int -> a -> IO (KudzuResult Integer)
testUntilSameLC n testable = do
    let rs = examineAndCount <$> LC.results testable
    r1 <- head rs
    grabUntilNSame 0 n n (tail rs) r1

examineAndCount :: ([String], Bool) -> IO Integer
examineAndCount v = unless (snd v) (error $ unwords ("test failed with:" : fst v)) >> tixModuleCount <$> examineTix

data KudzuResult a = KFail Int | KSuccess Int a

{-
Keep running property tests until the "amount" of code coverage is the same for N iterations of one test.
orig: the number of iterations of a test that must have the same amount of code coverage before you give up and stop.
c: the number of iterations checked
n: countdown to the success case
  -}
grabUntilNSame :: (Monad m, Eq a) => Int -> Int -> Int -> [m a] -> a -> m (KudzuResult a)
grabUntilNSame c _ 0 _ z = pure $ KSuccess c z -- we reached the desired window size
grabUntilNSame c _ _ [] _ = pure $ KFail c -- if we run out of list elements for test results, we're done
grabUntilNSame c orig n (a : as) z = do
    a' <- a
    if a' == z -- is the count of regions covered from this run the same as last run?
        then grabUntilNSame (c + 1) orig (n - 1) as z
        else grabUntilNSame (c + 1) orig orig as a'

-- where go c n a z =

-- | How many regions were executed at least once for this module?
tixCount :: TixModule -> Integer
tixCount (TixModule _ _ _ regions) = sum $ 1 <$ filter (> 0) regions

-- | How many regions were executed at least once for all these modules?
tixModuleCount :: Tix -> Integer
tixModuleCount (Tix ms) = sum $ map tixCount ms
