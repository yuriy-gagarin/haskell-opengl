{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Assets.Loading where

import Relude

import State
import Assets.Wavefront

loadAssets :: IO Resources
loadAssets = Resources 
  <$> readResource "assets/monkey.obj" 
  <*> readResource "assets/box.obj"
  <*> readResource "assets/torus.obj" 
  <*> readResource "assets/blob.obj"

readResource :: Text -> IO (Maybe Resource)
readResource path = 
  readFileText (toString path) <&> parseObj <&> convertToResource path