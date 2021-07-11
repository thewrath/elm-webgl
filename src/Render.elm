module Render exposing (renderSprite, renderSquare)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
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



-- Create a textured mesh transformed via the position matrix
-- @Todo take a record that contains all the matrices for the transformation of the mesh


textureMesh : Mat4 -> Mesh TextureVertex
textureMesh position =
    let
        topLeft =
            TextureVertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            TextureVertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            TextureVertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            TextureVertex (vec3 1 -1 1) (vec2 1 0)

        vertices =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]

        -- @Todo move in other function
        transformMat =
            position

        transform vertex =
            { vertex
                | position =
                    Mat4.transform transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
    List.map transformTriangle vertices |> WebGL.triangles



-- Render sprite in WebGL 2D context
-- @Todo use curry to enable modification of vertex and fragment shader


renderSprite : Float -> Float -> Texture -> Mat4 -> Entity
renderSprite x y texture camera =
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        (textureMesh (Mat4.identity |> Mat4.translate3 x y 1))
        { perspective = camera
        , texture = texture
        }



-- Render square in WebGL 2D context


renderSquare : Float -> Float -> Mat4 -> Entity
renderSquare x y camera =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = camera }
