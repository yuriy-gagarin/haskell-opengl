{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Misc.Utils where

import Relude
import qualified Relude.Unsafe as Unsafe
import qualified Text.Printf as Printf

import Control.Monad
import Graphics.Rendering.OpenGL.GL

len :: (Num n) => [a] -> n
len = fromIntegral . length
{-# INLINE len #-}

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

queue :: (Integral n) => n -> a -> [a] -> [a]
queue n element list = if len list < n then element : list else element : Unsafe.init list
{-# INLINE queue #-}