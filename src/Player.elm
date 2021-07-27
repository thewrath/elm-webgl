module Player exposing (..)

import Bullet exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Entity exposing (..)
import Json.Decode as Decode
import KeyHandler exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { entity : Entity.Model
    , speed : Float -- Speed in all direction
    , keyStates : KeyStates
    , bullets : List Bullet.Model
    , bulletPrototype : Bullet.Model
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        entity =
            Entity.empty mesh camera
                |> Entity.withPosition (vec2 50 50)
                |> Entity.withSize (vec2 32 32)
                |> Entity.withAngle 0
    in
    { entity = entity
    , speed = 4
    , keyStates = Dict.empty
    , bullets = []
    , bulletPrototype = Bullet.init mesh camera
    }


withPosition : Position -> Model -> Model
withPosition position model =
    { model | entity = Entity.withPosition position model.entity }


withKeyStates : KeyStates -> Model -> Model
withKeyStates keyStates model =
    { model | keyStates = keyStates }


onTexturesLoaded : TextureContainer -> Model -> Model
onTexturesLoaded textureContainer ({ bulletPrototype } as model) =
    { model
        | bulletPrototype = Bullet.withTexture "Bullet" textureContainer bulletPrototype
        , entity = Entity.withTexture "Player" textureContainer model.entity
    }


update : Model -> Model
update model =
    model
        |> move
        |> shoot



-- Move player depending on keyStates


move : Model -> Model
move ({ entity } as model) =
    let
        keyActions =
            Dict.fromList
                [ ( "ArrowUp", vec2 0 1 )
                , ( "ArrowRight", vec2 1 0 )
                , ( "ArrowDown", vec2 0 -1 )
                , ( "ArrowLeft", vec2 -1 0 )
                ]

        applyVelocity velocity =
            { model | entity = Entity.withPosition (Vec2.add entity.renderingProperties.position velocity) entity }
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



-- Shoot bullet on space down


shoot : Model -> Model
shoot model =
    -- check if space is down
    if Maybe.withDefault False (Dict.get " " model.keyStates) then
        model
        -- @Todo : clone bulletPrototype and add new instance in bullets list

    else
        model
