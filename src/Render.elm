module Render exposing (renderSprite, renderSquare)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shaders exposing (..)
import Transform exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)



-- Mesh


mesh : Mesh ColoredVertex
mesh =
    WebGL.triangles
        [ ( ColoredVertex (vec3 0 0 0) (vec3 1 1 1)
          , ColoredVertex (vec3 1 0 0) (vec3 1 1 1)
          , ColoredVertex (vec3 1 1 0) (vec3 1 1 1)
          )
        , ( ColoredVertex (vec3 0 0 0) (vec3 1 1 1)
          , ColoredVertex (vec3 0 1 0) (vec3 1 1 1)
          , ColoredVertex (vec3 1 1 0) (vec3 1 1 1)
          )
        ]



-- Create a textured mesh transformed via the position matrix
-- @Todo take a record that contains all the matrices for the transformation of the mesh


textureMesh : Mat4 -> Mesh TextureVertex
textureMesh transform =
    let
        topLeft =
            TextureVertex (vec3 -1 1 0) (vec2 0 1)

        topRight =
            TextureVertex (vec3 1 1 0) (vec2 1 1)

        bottomLeft =
            TextureVertex (vec3 -1 -1 0) (vec2 0 0)

        bottomRight =
            TextureVertex (vec3 1 -1 0) (vec2 1 0)

        vertices =
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]
    in
    List.map (transformTriangle transform transformVertex) vertices |> WebGL.triangles



-- Render sprite in WebGL 2D context
-- @Todo use curry to enable modification of vertex and fragment shader


renderSprite : Float -> Float -> Texture -> Mat4 -> Entity
renderSprite x y texture camera =
    let
        translation =
            Mat4.identity |> Mat4.translate3 x y 1

        rotation =
            Mat4.identity

        scale =
            Mat4.identity

        transform =
            Mat4.mul (Mat4.mul translation rotation) scale
    in
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        (textureMesh transform)
        { perspective = camera
        , texture = texture
        }



-- Render square in WebGL 2D context


renderSquare : Float -> Float -> Mat4 -> Entity
renderSquare x y camera =
    let
        translation =
            Mat4.identity |> Mat4.translate3 x y 1

        rotation =
            Mat4.identity

        scale =
            Mat4.makeScale (vec3 50 50 1)

        transform =
            Mat4.mul (Mat4.mul translation rotation) scale
    in
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = camera }
