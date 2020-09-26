{-# LANGUAGE NoImplicitPrelude #-}

module Aliased.StateVar 
  (
    getv
  , setv
  ) where

import Relude (MonadIO)

import Data.StateVar (HasGetter, HasSetter, ($=), get)

getv :: (MonadIO m, HasGetter t a) => t -> m a
getv = get
{-# INLINE getv #-}

setv :: (MonadIO m, HasSetter t a) => t -> a -> m ()
setv = ($=)
{-# INLINE setv #-}