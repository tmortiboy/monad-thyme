module Control.Monad.Thyme.ClassSpec (spec) where

import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State (evalStateT)
import           Test.Hspec

import           Control.Monad.Thyme.Class

spec :: Spec
spec =
  it "Should use instances correctly" $ do
    currentTime >>= print
    -- Test that generic MonadTrans instance works.
    runReaderT currentTime 'x' >>= print
    evalStateT (runReaderT currentTime 'x') 'y' >>= print
