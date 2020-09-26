{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Aliased.GL where

import Relude

import Foreign.Ptr
import Foreign.Storable
import Data.Vector.Storable as V
import Graphics.Rendering.OpenGL.GL as GL

import State
import Aliased.StateVar
import Misc.Utils

loadIntoBuffer :: (MonadIO m, MonadFail m) => Resource -> m GL.VertexArrayObject
loadIntoBuffer (Resource name vertices) = do

  let size = vsize vertices ?: 0

  when (size == 0) (fail "buffer loading failed")

  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer `setv` Just vertexBuffer
  liftIO $ unsafeWith vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer `setv` (size, ptr, GL.StaticDraw)

  vertexArray <- GL.genObjectName
  GL.bindVertexArrayObject `setv` Just vertexArray
  GL.vertexAttribArray (GL.AttribLocation 0) `setv` GL.Enabled
  GL.vertexAttribPointer (GL.AttribLocation 0) `setv` (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr)

  return vertexArray

vsize :: (Num n, Storable a) => Vector a -> Maybe n
vsize vector = do
  h <- headM vector
  return $ fromIntegral (sizeOf h * V.length vector)

lsize :: (Num n, Storable a) => [a] -> Maybe n
lsize list = do
  h <- viaNonEmpty Relude.head list
  return $ fromIntegral (sizeOf h * Relude.length list)