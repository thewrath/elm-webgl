module Player exposing (..)

import Debug exposing (..)
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)


type Direction
    = Up
    | Right
    | Down
    | Left
    | Other


type alias Model =
    { mesh : Mesh TextureVertex
    , angle : Float
    , texture : Maybe Texture
    , camera : Mat4
    , direction : Direction
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    { mesh = mesh, angle = 0, texture = Nothing, camera = camera, direction = Up }


withTextures : List Texture -> Model -> Model
withTextures textures model =
    case List.head textures of
        Nothing ->
            model

        Just texture ->
            { model | texture = Just texture }


keyCodeToDirection : String -> Direction
keyCodeToDirection keycode =
    case keycode of
        "ArrowUp" ->
            Up

        "ArrowRight" ->
            Right

        "ArrowDown" ->
            Down

        "ArrowLeft" ->
            Left

        _ ->
            Other


changeDirection : Model -> Direction -> Model
changeDirection model direction =
    { model | direction = Debug.log "player direction :" direction }


handleKeyDown : Model -> String -> Model
handleKeyDown model keycode =
    keyCodeToDirection keycode
        |> changeDirection model


handleKeyUp : Model -> String -> Model
handleKeyUp model _ =
    model


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
                spriteRenderingProperties =
                    RenderingProperties.empty
                        |> RenderingProperties.withPosition (vec2 50 50)
                        |> RenderingProperties.withSize (vec2 32 32)
                        |> RenderingProperties.withAngle model.angle
            in
            [ renderSprite model.mesh spriteRenderingProperties texture model.camera
            ]
