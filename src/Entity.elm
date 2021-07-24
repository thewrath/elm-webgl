module Entity exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Render exposing (..)
import RenderingProperties exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)



-- @Todo : abstract mesh declaration


type alias BaseModel =
    { texture : Maybe WebGL.Texture.Texture
    , mesh : Mesh TextureVertex
    , renderingProperties : RenderingProperties
    , camera : Mat4
    }


withTextures : String -> TextureContainer -> BaseModel -> BaseModel
withTextures textureName textures model =
    case Dict.get textureName textures of
        Nothing ->
            model

        Just texture ->
            { model | texture = Just texture }


view : BaseModel -> List WebGL.Entity
view model =
    case model.texture of
        Nothing ->
            []

        Just texture ->
            [ renderSprite model.mesh model.renderingProperties texture model.camera
            ]
