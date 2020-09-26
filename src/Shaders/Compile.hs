{-# LANGUAGE NoImplicitPrelude #-}

module Shaders.Compile
  (
    compileProgram
  ) where

import Relude
import qualified Graphics.Rendering.OpenGL.GL as GL

import Aliased.StateVar

compileShader :: GL.ShaderType -> ByteString -> IO GL.Shader
compileShader shaderType shaderSource = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader `setv` shaderSource

  GL.compileShader shader
  unlessM (getv (GL.compileStatus shader)) (getv (GL.shaderInfoLog shader) >>= fail)

  return shader

compileProgram :: ByteString -> ByteString -> IO GL.Program
compileProgram vertexShaderSource fragmentShaderSource = do
  vertexShader <- compileShader GL.VertexShader vertexShaderSource
  fragmentShader <- compileShader GL.FragmentShader fragmentShaderSource

  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader

  GL.linkProgram program
  unlessM (getv (GL.linkStatus program)) (getv (GL.programInfoLog program) >>= fail)

  return program