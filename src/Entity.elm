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


type alias Model =
    { texture : Maybe WebGL.Texture.Texture
    , mesh : Mesh TextureVertex
    , renderingProperties : RenderingProperties
    , camera : Mat4
    }


empty : Mesh TextureVertex -> Mat4 -> Model
empty mesh camera =
    { mesh = mesh
    , texture = Nothing
    , camera = camera
    , renderingProperties = RenderingProperties.empty
    }


withPosition : Position -> Model -> Model
withPosition position model =
    { model | renderingProperties = RenderingProperties.withPosition position model.renderingProperties }


withSize : Size -> Model -> Model
withSize size model =
    { model | renderingProperties = RenderingProperties.withSize size model.renderingProperties }


withAngle : Float -> Model -> Model
withAngle angle model =
    { model | renderingProperties = RenderingProperties.withAngle angle model.renderingProperties }


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
