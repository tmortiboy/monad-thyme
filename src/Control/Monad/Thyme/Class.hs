{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Control.Monad.Thyme.Class
  ( MonadTime(..)
  ) where

import           Control.Monad.Trans (MonadTrans(..))
import           Data.Thyme (UTCTime, getCurrentTime)

-- | Class of monads which carry the notion of the current time.
class Monad m => MonadTime m where
  currentTime :: m UTCTime

-- | Base instance for IO.
instance {-# OVERLAPPING #-} MonadTime IO where
  currentTime = getCurrentTime

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-} (MonadTime m, MonadTrans t, Monad (t m)) => MonadTime (t m) where
  currentTime = lift currentTime
