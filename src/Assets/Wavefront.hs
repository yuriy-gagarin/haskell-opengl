{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE LambdaCase        #-}

module Assets.Wavefront where

import Relude as R

import Unsafe.Coerce

import Data.Vector as V
import Data.Vector.Storable as VS
import Data.Attoparsec.Text as A
import Data.Sequence as S
import qualified Graphics.Rendering.OpenGL.GL as GL

import State (Resource(..))

data Vertex
data Face

data Wavefront
  = Vertex !Float !Float !Float !Float
  | Face !Int !Int !Int

eol :: Parser ()
eol = horizontalSpace *> endOfLine <|> endOfInput

comment :: Parser ()
comment = void (skipSpace *> "#" *> skipSpace *> manyTill anyChar eol)

unnecessary :: Parser ()
unnecessary = A.takeTill isEndOfLine *> endOfLine <|> endOfInput

horizontalSpace :: Parser ()
horizontalSpace = skipMany (satisfy isHorizontalSpace)

float :: Parser Float
float = fmap realToFrac double

vertex :: Parser Wavefront
vertex = horizontalSpace *> "v" *> horizontalSpace *> xyzw <* eol where
  xyzw = do
    floats <- float `sepBy1` horizontalSpace
    case floats of
      [x, y, z]    -> return (Vertex x y z 1)
      [x, y, z, w] -> return (Vertex x y z w)
      _ -> fail ""

face :: Parser Wavefront
face = horizontalSpace *> "f" *> horizontalSpace *> faces <* eol where
  faceIndices = decimal
  faces = do
    indices <- faceIndices `sepBy1` horizontalSpace
    case indices of
      [x, y, z] -> return (Face x y z)
      _ -> fail ""

combined :: Parser (Maybe Wavefront)
combined = Just    <$> vertex
       <|> Just    <$> face
       <|> Nothing <$  comment
       <|> Nothing <$  unnecessary

parseObj :: Text -> V.Vector Wavefront
parseObj text = parseOnly (combined `manyTill` endOfInput) text
            <&> catMaybes
             &  fromRight mempty
             &  V.fromList

splitInto :: V.Vector Wavefront -> (V.Vector Wavefront, V.Vector Wavefront)
splitInto wavefront = flip V.partition wavefront $ \case
  Vertex{} -> True
  Face{}   -> False

sortVertices :: V.Vector Wavefront -> V.Vector Wavefront -> VS.Vector (GL.Vector3 (GL.Vertex4 Float))
sortVertices vx faces = VS.generate (V.length faces) generator
  where
    generator ix = let (Face p q r) = faces V.! ix in GL.Vector3 (findVertex vx p) (findVertex vx q) (findVertex vx r)

findVertex :: V.Vector Wavefront -> Int -> GL.Vertex4 Float
findVertex vertices ix = case vertices V.!? (ix-1) of
      Just (Vertex x y z w) -> GL.Vertex4 x y z w
      Nothing               -> GL.Vertex4 0 0 0 0

convertToResource :: Text -> V.Vector Wavefront -> Maybe Resource
convertToResource name wavefront = Resource name sortedVertices & Just where
  sortedVertices = splitInto wavefront 
                 & uncurry sortVertices