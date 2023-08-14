{-# LANGUAGE OverloadedStrings #-}

module Kudzu where

import Control.Monad (unless)
import qualified Hedgehog as HH
import qualified Test.LeanCheck as LC
import qualified Test.QuickCheck as QC
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..))

testUntilSameQCMany :: (Traversable t, QC.Testable a) => Int -> t a -> IO (t (Int, Maybe Integer))
testUntilSameQCMany howMany ts = do
  mapM (testUntilSameQC howMany) ts

-- | QuickCheck
testUntilSameQC :: QC.Testable a => Int -> a -> IO (Int, Maybe Integer)
testUntilSameQC n testable = do
  let rs = examineAndCount' <$> repeat testable
  r1 <- head rs
  grabUntilNSame 0 n n (tail rs) r1

examineAndCount' :: QC.Testable prop => prop -> IO Integer
examineAndCount' v = do
  -- poor QC, you got problems
  -- quickCheckWith (stdArgs {maxSize = 400, maxSuccess = 1}) v
  QC.quickCheck (QC.withMaxSuccess 1 v)
  tixModuleCount <$> examineTix

-- | Hedgehog
testUntilSameHHMany :: Traversable t => Int -> t HH.Property -> IO (t (Int, Maybe Integer))
testUntilSameHHMany howMany ps = do
  mapM (testUntilSameHH howMany) ps

testUntilSameHH :: Int -> HH.Property -> IO (Int, Maybe Integer)
testUntilSameHH n prop = do
  let rs = examineAndCountHH <$> repeat prop
  r1 <- head rs
  grabUntilNSame 0 n n (tail rs) r1

examineAndCountHH :: HH.Property -> IO Integer
examineAndCountHH prop = do
  passed <- HH.check prop
  unless passed $ error "property failed"
  tixModuleCount <$> examineTix

testUntilSameLCMany :: (Traversable t, LC.Testable a) => Int -> t a -> IO (t (Int, Maybe Integer))
testUntilSameLCMany howMany ts = do
  mapM (testUntilSameLC howMany) ts

-- | LeanCheck
testUntilSameLC :: LC.Testable a => Int -> a -> IO (Int, Maybe Integer)
testUntilSameLC n testable = do
  let rs = examineAndCount <$> LC.results testable
  r1 <- head rs
  grabUntilNSame 0 n n (tail rs) r1

examineAndCount :: ([String], Bool) -> IO Integer
examineAndCount v = unless (snd v) (error $ unwords ("failed with:":fst v)) >> tixModuleCount <$> examineTix

grabUntilNSame :: (Monad m, Eq a) => Int -> Int -> Int -> [m a] -> a -> m (Int, Maybe a)
grabUntilNSame c _ 0 _ z = pure (c, Just z)
grabUntilNSame c _ _ [] _ = pure (c, Nothing)
grabUntilNSame c orig n (a : as) z = do
  a' <- a
  if a' == z
    then grabUntilNSame (c + 1) orig (n - 1) as z
    else grabUntilNSame (c + 1) orig orig as a'

-- How many regions were executed at least once for this module?
tixCount :: TixModule -> Integer
tixCount (TixModule _ _ _ regions) = sum $ 1 <$ filter (> 0) regions

-- How many regions were executed at least once for all these modules?
tixModuleCount :: Tix -> Integer
tixModuleCount (Tix ms) = sum $ map tixCount ms
