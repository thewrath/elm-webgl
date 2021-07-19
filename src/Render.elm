module Render exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import RenderingProperties exposing (..)
import Shaders exposing (..)
import Transform exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings exposing (colorMask)
import WebGL.Texture as Texture exposing (Texture)



-- Mesh


type alias MeshBank =
    { coloredMesh : Mesh ColoredVertex
    , textureMesh : Mesh TextureVertex
    }


initMeshBank : MeshBank
initMeshBank =
    { coloredMesh = coloredMesh
    , textureMesh = textureMesh
    }


coloredMesh : Mesh ColoredVertex
coloredMesh =
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
    vertices |> WebGL.triangles



-- Create a textured mesh transformed via the position matrix
-- @Todo take a record that contains all the matrices for the transformation of the mesh


textureMesh : Mesh TextureVertex
textureMesh =
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
    vertices |> WebGL.triangles



-- Render
-- Render sprite in WebGL 2D context


renderSprite : Mesh TextureVertex -> RenderingProperties -> Texture -> Camera -> Entity
renderSprite mesh renderingProperties texture camera =
    WebGL.entityWith
        [ WebGL.Settings.sampleAlphaToCoverage ]
        texturedVertexShader
        texturedFragmentShader
        mesh
        { perspective = camera
        , transform = transformRenderingProperties renderingProperties
        , texture = texture
        }



-- Render square in WebGL 2D context


renderSquare : Mesh ColoredVertex -> RenderingProperties -> Camera -> Entity
renderSquare mesh renderingProperties camera =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        { perspective = camera
        , transform = transformRenderingProperties renderingProperties
        }
