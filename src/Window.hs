{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Window where

import Relude
import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.GL as Raw
import Lens.Micro.Platform

import qualified Data.Vector.Storable as V
import Foreign.Ptr
import Foreign.Storable
import Text.Pretty.Simple

import Text.Printf (printf)

import Aliased.StateVar
import Aliased.GL

import Constants
import Events
import State
import Misc.Utils
import GameM ()

initializeWindow :: MonadIO m => m (SDL.Window, SDL.GLContext)
initializeWindow = do
  SDL.initializeAll

  window  <- SDL.createWindow "GLWindow" defaultWindowConfig
  context <- SDL.glCreateContext window

  SDL.swapInterval `setv` SDL.ImmediateUpdates

  return (window, context)

mainLoop :: (MonadState AppState m, MonadIO m, MonadFail m) => m ()
mainLoop = do
  startT <- SDL.ticks

  window          <- use (context.window)
  framesToAverage <- use (config.framesToAverage)
  totalFrameCount <- use (gameState.framerate.totalFrameCount)

  when (totalFrameCount `mod` framesToAverage == 0) printFrameData

  GL.clearColor `setv` GL.Color4 0.2 0.2 0.2 0
  GL.depthFunc `setv` Just GL.Less

  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  program <- use (context.program)
  GL.currentProgram `setv` Just program

  Just shape  <- use (resources.monkey)
  -- Just shape  <- use (resources.blob)

  vertexArrayObject <- loadIntoBuffer shape
  GL.bindVertexArrayObject `setv` Just vertexArrayObject

  timeLoc <- getv (GL.uniformLocation program "time")

  GL.uniform timeLoc `setv` (fi startT :: GL.GLfloat)

  liftIO $ GL.drawArrays GL.Triangles 0 9999

  events <- SDL.pollEvents

  SDL.glSwapWindow window

  endT   <- SDL.ticks
  let
    elapsed = fromIntegral (endT - startT)
    delay   = millisecondsPerFrame - elapsed

  modifying (gameState.framerate) (advanceFrameState framesToAverage elapsed delay)
  when (delay > 0) (SDL.delay (floor delay))
  unless (any applicationQuit events) mainLoop

advanceFrameState :: Word -> Double -> Double -> Framerate -> Framerate
advanceFrameState frames elapsed delay =
      elapsedTimes %~ queue frames elapsed
  >>> delayedTimes %~ queue frames delay
  >>> totalTimes   %~ queue frames (elapsed + delay)
  >>> totalFrameCount +~ 1

printFrameData :: (MonadState AppState m, MonadIO m) => m ()
printFrameData = do

  totalFrameCount <- use (gameState.framerate.totalFrameCount)
  elapsedTimes    <- use (gameState.framerate.elapsedTimes)
  delayedTimes    <- use (gameState.framerate.delayedTimes)
  totalTimes      <- use (gameState.framerate.totalTimes)

  let
    avg xs = sum xs / len xs
    elapsedM = avg elapsedTimes
    delayedM = avg delayedTimes
    totalM   = avg totalTimes
  
  liftIO $ printf "frame %d / %.4f ms / %.4f ms / %.4f ms\n" totalFrameCount elapsedM delayedM totalM