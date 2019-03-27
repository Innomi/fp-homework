{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2_1
  ( MonadJoin (..)
  ) where

import Prelude hiding (Monad (..))

-- Task 3 continue

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

instance Monad m => MonadJoin m where
  returnJoin = return

  join = (>>= id)
