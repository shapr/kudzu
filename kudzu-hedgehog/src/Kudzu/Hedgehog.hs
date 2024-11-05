{-# LANGUAGE OverloadedStrings #-}

module Kudzu.Hedgehog where

import Control.Monad (unless)
import qualified Hedgehog as HH
import Kudzu.Core (KudzuResult, examineTix, grabUntilNSame, tixModuleCount)

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
