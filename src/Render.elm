module Render exposing (renderSprite)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shaders exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)



-- Mesh


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 1 1)
          , Vertex (vec3 1 0 0) (vec3 1 1 1)
          , Vertex (vec3 1 1 0) (vec3 1 1 1)
          )
        , ( Vertex (vec3 0 0 0) (vec3 1 1 1)
          , Vertex (vec3 0 1 0) (vec3 1 1 1)
          , Vertex (vec3 1 1 0) (vec3 1 1 1)
          )
        ]



-- Function to render sprite
-- @Todo use curry to enable modification of vertex and fragment shader
-- @Todo enable modification of position
-- @Todo render sprite instead of rectangle


renderSprite : Float -> Float -> String -> Mat4 -> Entity
renderSprite x y image camera =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = Mat4.makeOrtho2D 0 800 0 800 }
