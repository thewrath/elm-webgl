module Render exposing (renderSprite)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shaders exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)



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


textureMesh : Mesh TextureVertex
textureMesh =
    WebGL.triangles
        [ ( TextureVertex (vec3 0 0 0) (vec2 1 1)
          , TextureVertex (vec3 1 0 0) (vec2 1 1)
          , TextureVertex (vec3 1 1 0) (vec2 1 1)
          )
        , ( TextureVertex (vec3 0 0 0) (vec2 1 1)
          , TextureVertex (vec3 0 1 0) (vec2 1 1)
          , TextureVertex (vec3 1 1 0) (vec2 1 1)
          )
        ]



-- Render sprite in WebGL 2D context
-- @Todo use curry to enable modification of vertex and fragment shader
-- @Todo enable modification of position


renderSprite : Float -> Float -> Texture -> Mat4 -> Entity
renderSprite x y texture camera =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        textureMesh
        { perspective = camera
        , texture = texture
        }



-- Render square in WebGL 2D context


renderSquare : Float -> Float -> String -> Mat4 -> Entity
renderSquare x y image camera =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = camera }
