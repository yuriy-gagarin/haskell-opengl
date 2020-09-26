{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Misc.QuasiQuotes where

import Relude

import Language.Haskell.TH.Quote

s :: QuasiQuoter
s = QuasiQuoter 
  (\x -> [|fromString x|])
  (error "cannot be used as a pattern")
  (error "cannot be uses as a type") 
  (error "cannot be used as a declaration")