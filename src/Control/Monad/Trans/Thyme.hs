{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Thyme
  ( TimeT
  , runTimeT
  , runTimeT'
  ) where

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Morph (MFunctor)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.State (StateT, evalStateT, get, put)
import           Control.Monad.Trans (MonadIO, MonadTrans)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Thyme (UTCTime)

import           Control.Monad.Thyme.Class (MonadTime(..))

-- | A mock time transformer monad which adds a collection of 'UTCTime's to the given monad,
-- which are used by 'currentTime'.
newtype TimeT m a =
  TimeT (StateT (NonEmpty UTCTime) m a)
  deriving (Functor, Applicative, MFunctor, Monad, MonadIO, MonadThrow, MonadCatch, MonadTrans,
           MonadBase b)

instance {-# OVERLAPPING #-} Monad m => MonadTime (TimeT m) where
  currentTime = do
    (time, rest) <- NE.uncons <$> TimeT get
    maybe (pure ()) (TimeT . put) rest -- update the state only if there are more times
    pure time

instance PrimMonad m => PrimMonad (TimeT m) where
  type PrimState (TimeT m) = PrimState m
  primitive f = TimeT $ primitive f

-- | Runs a 'TimeT' and extracts the final value from it.
-- Uses the same provided value for all calls to 'currentTime'.
runTimeT ::
  Monad m
  => UTCTime   -- ^ An @UTCTime@ to be used for all responses for @currentTime'.
  -> TimeT m a -- ^ A @TimeT@ to run.
  -> m a
runTimeT utcTime = runTimeT' $ utcTime :| []
{-# INLINE runTimeT #-}

-- | Runs a 'TimeT' and extracts the final value from it.
-- Uses the each provided value in turn for all calls to 'currentTime'.
-- If more calls to 'currentTime' than provided values, the last provided value will be used,
-- meaning that @runTimeT myUTCTime@ and @runTimeT' $ myUTCTime :| []@ are equivalent:
runTimeT' ::
  Monad m
  => NonEmpty UTCTime -- ^ A collection of @UTCTime@s to be used in order for responses for @currentTime'.
  -> TimeT m a        -- ^ A @TimeT@ to run.
  -> m a
runTimeT' utcTimes (TimeT s) = evalStateT s utcTimes
{-# INLINE runTimeT' #-}
