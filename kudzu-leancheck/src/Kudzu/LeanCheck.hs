{-# LANGUAGE OverloadedStrings #-}

module Kudzu.LeanCheck where

import Control.Monad (unless)
import Kudzu.Core (KudzuResult, examineTix, grabUntilNSame, tixModuleCount)
import qualified Test.LeanCheck as LC

testUntilSameLCMany :: (Traversable t, LC.Testable a) => Int -> t a -> IO (t (KudzuResult Integer))
testUntilSameLCMany howMany ts = do
    mapM (testUntilSameLC howMany) ts

testUntilSameLC :: (LC.Testable a) => Int -> a -> IO (KudzuResult Integer)
testUntilSameLC n testable = grabUntilNSame n $ examineAndCount <$> LC.results testable

examineAndCount :: ([String], Bool) -> IO Integer
examineAndCount v = unless (snd v) (error $ unwords ("test failed with:" : fst v)) >> tixModuleCount <$> examineTix
