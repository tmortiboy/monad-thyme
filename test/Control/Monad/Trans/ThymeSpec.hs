module Control.Monad.Trans.ThymeSpec (spec) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Control.Monad.Thyme
import           Control.Monad.Trans.Thyme

spec :: Spec
spec = do
  context "runTimeT" $ do

    prop "Should use provided value for currentTime" $ \utcTime -> do
      result <- runTimeT utcTime currentTime
      result `shouldBe` utcTime

    prop "Should always use provided value for all currentTime calls" $ \utcTime (Positive n) -> do
      result <- runTimeT utcTime $ sequence $ replicate n currentTime
      result `shouldSatisfy` all (== utcTime)

  context "runTimeT'" $ do

    prop "Should use provided values for currentTime" $ \utcTime1 utcTime2 utcTime3 -> do
      (result1, result2, result3) <- runTimeT' (utcTime1 :| [utcTime2, utcTime3]) $
        (,,) <$> currentTime <*> currentTime <*> currentTime
      result1 `shouldBe` utcTime1
      result2 `shouldBe` utcTime2
      result3 `shouldBe` utcTime3

    prop "Should use last provided value when more currentTime calls than values" $
      \utcTime1 utcTime2 utcTime3 -> do
        (result1, result2, result3, result4, result5) <-
          runTimeT' (utcTime1 :| [utcTime2, utcTime3]) $
            (,,,,) <$> currentTime <*> currentTime <*> currentTime <*> currentTime <*> currentTime
        result1 `shouldBe` utcTime1
        result2 `shouldBe` utcTime2
        result3 `shouldBe` utcTime3
        result4 `shouldBe` utcTime3
        result5 `shouldBe` utcTime3

    prop "Should always use single provided value for all currentTime calls" $
      \utcTime (Positive n) -> do
        result <- runTimeT' (utcTime :| []) $ sequence $ replicate n currentTime
        result `shouldSatisfy` all (== utcTime)
