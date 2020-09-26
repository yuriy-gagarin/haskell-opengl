{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module State where

import Relude

import Data.Vector.Storable
import Lens.Micro.TH
import qualified SDL
import qualified Graphics.Rendering.OpenGL.GL as GL

data Context = Context
  { _window  :: SDL.Window
  , _program :: GL.Program
  }
  deriving Show

data Resource = Resource !Text !(Vector (GL.Vector3 (GL.Vertex4 GL.GLfloat)))
  deriving Show

data Resources = Resources
  { _monkey :: Maybe Resource
  , _box    :: Maybe Resource
  , _torus  :: Maybe Resource
  , _blob   :: Maybe Resource
  }
  deriving Show

newtype Config = Config
  { _framesToAverage :: Word
  }
  deriving Show

data Framerate = Framerate
  { _elapsedTimes    :: [Double]
  , _delayedTimes    :: [Double]
  , _totalTimes      :: [Double]
  , _totalFrameCount :: Word
  }
  deriving Show

newtype GameState = GameState
  { _framerate :: Framerate
  }
  deriving Show

data AppState = AppState
  { _context     :: !Context
  , _resources   :: !Resources
  , _config      :: !Config
  , _gameState   :: !GameState
  }
  deriving Show

makeLenses ''AppState
makeLenses ''GameState
makeLenses ''Framerate
makeLenses ''Config
makeLenses ''Resources
makeLenses ''Context

createAppState :: Context -> Resources -> AppState
createAppState context resources = AppState context resources defaultConfig defaultGameState

defaultResources = Resources Nothing
defaultConfig    = Config 60
defaultGameState = GameState (Framerate [] [] [] 0)