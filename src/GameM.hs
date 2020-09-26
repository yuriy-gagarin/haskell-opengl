{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}

module GameM where

import Relude
import Lens.Micro.Platform

import qualified State

newtype GameM a = GameM (StateT State.AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState State.AppState)

runGameM :: GameM a -> State.AppState -> IO ()
runGameM (GameM game) appState = void (runStateT game appState)