module Render exposing (renderSprite, renderSquare)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shaders exposing (..)
import Transform exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)



-- Mesh


coloredMesh : Mat4 -> Mesh ColoredVertex
coloredMesh transform =
    let
        vertices =
            [ ( ColoredVertex (vec3 0 0 0) (vec3 1 1 1)
              , ColoredVertex (vec3 1 0 0) (vec3 1 1 1)
              , ColoredVertex (vec3 1 1 0) (vec3 1 1 1)
              )
            , ( ColoredVertex (vec3 0 0 0) (vec3 1 1 1)
              , ColoredVertex (vec3 0 1 0) (vec3 1 1 1)
              , ColoredVertex (vec3 1 1 0) (vec3 1 1 1)
              )
            ]
    in
    List.map (transformTriangle transform transformVertex) vertices |> WebGL.triangles



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


renderSprite : Position -> Size -> Float -> Texture -> Camera -> Entity
renderSprite position size rotation texture camera =
    let
        transform =
            Mat4.identity
                |> Mat4.translate (vec3 (getX position) (getY position) 1)
                |> Mat4.rotate rotation (vec3 0 0 1)
                |> Mat4.scale (vec3 (getX size) (getY size) 1)
    in
    WebGL.entity
        texturedVertexShader
        texturedFragmentShader
        (textureMesh transform)
        { perspective = camera
        , texture = texture
        }



-- Render square in WebGL 2D context


renderSquare : Position -> Size -> Float -> Camera -> Entity
renderSquare position size rotation camera =
    let
        transform =
            Mat4.identity
                |> Mat4.translate (vec3 (getX position) (getY position) 1)
                |> Mat4.rotate rotation (vec3 0 0 1)
                |> Mat4.scale (vec3 (getX size) (getY size) 1)
    in
    WebGL.entity
        vertexShader
        fragmentShader
        (coloredMesh transform)
        { perspective = camera }
