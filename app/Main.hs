{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Relude

import GHC.IO.Encoding

import Window
import State
import GameM

import Assets.Loading
import Shaders.Compile
import qualified Shaders.Source as Source

import Halive.Utils

import Assets.Wavefront

main :: IO ()
main = do
  setLocaleEncoding utf8

  (window, context) <- reacquire "window" initializeWindow

  program <- compileProgram Source.vertex Source.fragment

  resources <- loadAssets
  runGameM mainLoop $ createAppState (Context window program) resources