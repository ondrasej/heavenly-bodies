-- Copyright 2017 Ondrej Sykora
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module HBodies.GLUtils
    (
      glPosition
    , glSize

    , color3D
    , color4D

    , vector2D
    , vector3D

    , vertex2D
    , vertex3D

    , setUpMatrixFromPosition
    ) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified HBodies.Geometry as Geometry

-- | Converts an integer to any other integral type.
convert :: (Integral a, Num b) => a -> b
convert = fromInteger . toInteger

-- | Converts radians to degrees.
toDegrees :: Double -> Double
toDegrees radians = radians * 180.0 / pi

-- | Creates a GL.Position object from the given coordinates.
glPosition :: Integral a => a -> a -> GL.Position
glPosition x y = GL.Position (convert x) (convert y)

-- | Creates a GL.Size object from the given coordinates.
glSize :: Integral a => a -> a -> GL.Size
glSize x y = GL.Size (convert x) (convert y)

-- | Creates a rotation vector for rotating the model (in 2D) by the given
-- angle.
rotationVector :: GL.Vector3 GL.GLdouble
rotationVector = GL.Vector3 0.0 0.0 1.0

-- | Sets up the translation and rotation matrix for rendering an object.
setUpMatrixFromPosition :: Geometry.Position -> IO ()
setUpMatrixFromPosition position = do
    let x = Geometry.getX position
        y = Geometry.getY position
        rotation = Geometry.getRotation position
    GL.translate$ vector3D x y 0.0
    GL.rotate (realToFrac $ toDegrees rotation) rotationVector

-- | Creates a color object from the color components.
color3D :: Double -> Double -> Double -> GL.Color3 GL.GLdouble
color3D r g b = GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b)

color4D :: Double -> Double -> Double -> Double -> GL.Color4 GL.GLdouble
color4D r g b a =
    GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | Creates a 2D vector object from the given coordinates.
vector2D :: Double -> Double -> GL.Vector3 GL.GLdouble
vector2D x y = GL.Vector3 (realToFrac x) (realToFrac y) 0.0

-- | Creates a 3D vector object from the given coordinates.
vector3D :: Double -> Double -> Double -> GL.Vector3 GL.GLdouble
vector3D x y z = GL.Vector3 (realToFrac x)
                            (realToFrac y)
                            (realToFrac z)

-- | Creates a 2D vertex object from the given coordinates.
vertex2D :: Double -> Double -> GL.Vertex2 GL.GLdouble
vertex2D x y = GL.Vertex2 (realToFrac x) (realToFrac y)

-- | Creates a 3D vertex object from the given coordinates.
vertex3D :: Double -> Double -> Double -> GL.Vertex3 GL.GLdouble
vertex3D x y z = GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)
