{-# LANGUAGE NoImplicitPrelude #-}

module Constants where

import Relude
import SDL

microsecondsPerFrame = 1000000/30
millisecondsPerFrame = microsecondsPerFrame/1000
defaultWindowConfig  = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL { glProfile = Core Normal 4 1} }