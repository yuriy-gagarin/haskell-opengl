{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Events where

import Relude
import SDL

applicationQuit :: SDL.Event -> Bool
applicationQuit (eventPayload -> QuitEvent) = True
applicationQuit _ = False