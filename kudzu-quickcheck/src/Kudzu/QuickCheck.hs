module Kudzu.QuickCheck where

import Kudzu.Core (KudzuResult, examineTix, grabUntilNSame, tixModuleCount)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Random as QC

testUntilSameQCMany :: (Traversable t, QC.Testable a) => Int -> t a -> IO (t (KudzuResult Integer))
testUntilSameQCMany howMany ts = do
    mapM (testUntilSameQC howMany) ts

testUntilSameQC :: (QC.Testable a) => Int -> a -> IO (KudzuResult Integer)
testUntilSameQC n testable = do
    let rs = map (examineAndCount' testable) [0 .. n]
    grabUntilNSame n rs

examineAndCount' :: (QC.Testable prop) => prop -> Int -> IO Integer
examineAndCount' v size = do
    qcg <- QC.newQCGen
    QC.quickCheckWith (QC.stdArgs{QC.replay = Just (qcg, size)}) (QC.withMaxSuccess 1 v)
    tixModuleCount <$> examineTix
