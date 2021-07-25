module Entity exposing (..)

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)



-- @Todo : abstract mesh declaration
-- @Todo : use builder pattern https://sporto.github.io/elm-patterns/basic/builder-pattern.html


type alias Model =
    { texture : Maybe WebGL.Texture.Texture
    , mesh : Mesh TextureVertex
    , renderingProperties : RenderingProperties
    , camera : Mat4
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        renderingProperties =
            RenderingProperties.empty
                |> RenderingProperties.withPosition (vec2 50 50)
                |> RenderingProperties.withSize (vec2 32 32)
                |> RenderingProperties.withAngle 0
    in
    { mesh = mesh
    , texture = Nothing
    , camera = camera
    , renderingProperties = renderingProperties
    }


withTexture : String -> TextureContainer -> Model -> Model
withTexture textureName textures model =
    { model | texture = Dict.get textureName textures }


view : Model -> List WebGL.Entity
view model =
    case model.texture of
        Nothing ->
            []

        Just texture ->
            [ renderSprite model.mesh model.renderingProperties texture model.camera
            ]
