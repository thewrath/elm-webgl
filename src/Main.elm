module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Debug
import Enemy exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, add, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Player exposing (..)
import Render exposing (..)
import RenderingProperties exposing (..)
import Shaders exposing (..)
import Task exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Options, Texture, linear, nearest, repeat)



-- Application Model (textures)


type alias Model =
    { textures : Maybe (List Texture)
    , meshBank : MeshBank
    , enemyModel : Enemy.Model
    , playerModel : Player.Model
    }



-- Application Action (message in Elm), define action that can be performed by the runtime "asynchronously"


type Action
    = TexturesError Error
    | TexturesLoaded (List Texture)
    | AnimationFrame Float
    | OnKeyDown Player.Direction


main : Program Value Model Action
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- Initialize application (fetch textures, ...)


init : flags -> ( Model, Cmd Action )
init _ =
    let
        meshBank =
            initMeshBank

        textureOptions =
            { magnify = nearest
            , minify = nearest
            , horizontalWrap = repeat
            , verticalWrap = repeat
            , flipY = True
            }

        textures =
            [ "../textures/shmup/shmup/color/alien1.png", "../textures/thwomp-side.jpg" ]

        -- @Todo : instead of building the records here it would be better to use the factory pattern in the Player and Enemy modules
        enemyModel =
            { mesh = initMeshBank.textureMesh
            , angle = 0
            , texture = Nothing
            , camera = orthographicCamera
            }

        playerModel =
            { mesh = initMeshBank.textureMesh
            , angle = 0
            , texture = Nothing
            , camera = orthographicCamera
            , direction = Player.Up
            }
    in
    ( { textures = Nothing
      , meshBank = meshBank
      , enemyModel = enemyModel
      , playerModel = playerModel
      }
    , Cmd.batch [ loadTextures textureOptions textures ]
    )



-- Update function, handle message and bring your entity to life


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TexturesLoaded textures ->
            ( { model
                | textures = Just textures
                , enemyModel = Enemy.withTextures textures model.enemyModel
                , playerModel = Player.withTextures textures model.playerModel
              }
            , Cmd.none
            )

        TexturesError err ->
            ( model, Cmd.none )

        AnimationFrame delta ->
            ( { model | enemyModel = Enemy.update model.enemyModel }, Cmd.none )

        OnKeyDown direction ->
            ( { model | playerModel = Player.changeDirection direction model.playerModel }, Cmd.none )



-- Draw stuff here


view : Model -> Html msg
view model =
    case model.textures of
        -- textures not loaded
        Nothing ->
            text "Loading ..."

        Just textures ->
            WebGL.toHtml
                [ width 800
                , height 800
                , style "display" "block"
                , style "background-color" "black"
                , style "margin" "auto"
                ]
                (List.concat [ Enemy.view model.enemyModel, Player.view model.playerModel ])


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\f -> AnimationFrame f)
        , onKeyDown (Decode.map OnKeyDown Player.keyDecoder)
        ]


orthographicCamera : Mat4
orthographicCamera =
    Mat4.makeOrtho2D 0 800 0 800



-- Tell elm runtime to perform tasks that load textures
-- @Todo move in submodule


loadTextures : Options -> List String -> Cmd Action
loadTextures options textureNames =
    textureNames
        |> List.map (Texture.loadWith options)
        -- turn textures into list of "loading texture task"
        |> Task.sequence
        -- create a new sequence task (task compose of a list of tasks)
        |> Task.andThen Task.succeed
        -- Turn task result into message that can be handle in update function
        |> Task.attempt
            (\result ->
                case result of
                    Err error ->
                        TexturesError error

                    Ok textures ->
                        TexturesLoaded textures
            )
