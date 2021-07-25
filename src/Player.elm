module Player exposing (..)

import Bullet exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Entity exposing (..)
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { mesh : Mesh TextureVertex
    , speed : Float -- Speed in all direction
    , texture : Maybe WebGL.Texture.Texture
    , camera : Mat4
    , keyStates : Dict String Bool
    , renderingProperties : RenderingProperties
    , bullets : List Bullet.Model
    , bulletPrototype : Bullet.Model
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        renderingProperties =
            RenderingProperties.empty
                |> RenderingProperties.withPosition (vec2 0 0)
                |> RenderingProperties.withSize (vec2 32 32)
                |> RenderingProperties.withAngle 0
    in
    { mesh = mesh
    , speed = 4
    , texture = Nothing
    , camera = camera
    , keyStates = Dict.empty
    , renderingProperties = renderingProperties
    , bullets = []
    , bulletPrototype = Bullet.init mesh camera
    }


onTexturesLoaded : TextureContainer -> Model -> Model
onTexturesLoaded textureContainer ({ bulletPrototype } as model) =
    { model | bulletPrototype = Bullet.withTexture "Bullet" textureContainer bulletPrototype }
        |> withTexture "Player" textureContainer


toEntity : Model -> Entity.Model
toEntity model =
    { texture = model.texture
    , mesh = model.mesh
    , camera = model.camera
    , renderingProperties = model.renderingProperties
    }


withTexture : String -> TextureContainer -> Model -> Model
withTexture textureName textures model =
    { model | texture = Dict.get textureName textures }


withPosition : Position -> Model -> Model
withPosition position model =
    { model | renderingProperties = RenderingProperties.withPosition position model.renderingProperties }


handleKeyDown : Model -> String -> Model
handleKeyDown model keycode =
    { model | keyStates = Dict.insert keycode True model.keyStates }


handleKeyUp : Model -> String -> Model
handleKeyUp model keycode =
    { model | keyStates = Dict.insert keycode False model.keyStates }


update : Model -> Model
update model =
    model
        |> move
        |> shoot



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
            { model | renderingProperties = RenderingProperties.withPosition (Vec2.add model.renderingProperties.position velocity) model.renderingProperties }
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
