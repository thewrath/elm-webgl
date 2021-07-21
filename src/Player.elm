module Player exposing (..)

import Debug exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { mesh : Mesh TextureVertex
    , angle : Float
    , position : Position
    , speed : Float -- Speed in all direction
    , texture : Maybe WebGL.Texture.Texture
    , camera : Mat4
    , keyStates : Dict String Bool
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    { mesh = mesh
    , angle = 0
    , position = vec2 0 0
    , speed = 4
    , texture = Nothing
    , camera = camera
    , keyStates = Dict.empty
    }



-- @todo replace with texture container type


withTextures : TextureContainer -> Model -> Model
withTextures textures model =
    case Dict.get "Player" textures of
        Nothing ->
            model

        Just texture ->
            { model | texture = Just texture }


withPosition : Position -> Model -> Model
withPosition position model =
    { model | position = position }


handleKeyDown : Model -> String -> Model
handleKeyDown model keycode =
    { model | keyStates = Dict.insert keycode True model.keyStates }


handleKeyUp : Model -> String -> Model
handleKeyUp model keycode =
    { model | keyStates = Dict.insert keycode False model.keyStates }


update : Model -> Model
update model =
    model |> move



-- Move player depending on keyStates


move : Model -> Model
move model =
    let
        keyActions =
            Dict.fromList
                [ ( "ArrowUp", vec2 0 1 )
                , ( "ArrowRight", vec2 1 0 )
                , ( "ArrowDown", vec2 0 -1 )
                , ( "ArrowLeft", vec2 -1 0 )
                ]

        applyVelocity velocity =
            { model | position = Vec2.add model.position velocity }
    in
    Dict.keys keyActions
        |> List.map
            (\key ->
                Maybe.withDefault False (Dict.get key model.keyStates)
                    |> (\keyState ->
                            if keyState == True then
                                Maybe.withDefault (vec2 0 0) (Dict.get key keyActions)

                            else
                                vec2 0 0
                       )
            )
        |> List.foldr Vec2.add (vec2 0 0)
        |> Vec2.scale model.speed
        -- Add correction of higher speed in diagonal direction
        |> applyVelocity


view : Model -> List WebGL.Entity
view model =
    case model.texture of
        Nothing ->
            []

        Just texture ->
            let
                spriteRenderingProperties =
                    RenderingProperties.empty
                        |> RenderingProperties.withPosition model.position
                        |> RenderingProperties.withSize (vec2 32 32)
                        |> RenderingProperties.withAngle model.angle
            in
            [ renderSprite model.mesh spriteRenderingProperties texture model.camera
            ]
