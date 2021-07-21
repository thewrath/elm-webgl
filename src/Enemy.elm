-- @Todo : Share code with Player module


module Enemy exposing (..)

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)


type alias Model =
    { mesh : Mesh TextureVertex
    , angle : Float
    , texture : Maybe Texture
    , camera : Mat4
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    { mesh = mesh, angle = 0, texture = Nothing, camera = camera }


withTextures : Dict String Texture -> Model -> Model
withTextures textures model =
    case Dict.get "Alien" textures of
        Nothing ->
            model

        Just texture ->
            { model | texture = Just texture }


update : Model -> Model
update model =
    { model | angle = model.angle + 0.05 }


view : Model -> List WebGL.Entity
view model =
    case model.texture of
        Nothing ->
            []

        Just texture ->
            let
                spriteRenderingProperties position =
                    RenderingProperties.empty
                        |> RenderingProperties.withPosition position
                        |> RenderingProperties.withSize (vec2 32 32)
                        |> RenderingProperties.withAngle model.angle
            in
            [ spriteRenderingProperties (vec2 50 50)
            , spriteRenderingProperties (vec2 50 750)
            , spriteRenderingProperties (vec2 750 50)
            , spriteRenderingProperties (vec2 750 750)
            ]
                |> List.map (\rp -> renderSprite model.mesh rp texture model.camera)
