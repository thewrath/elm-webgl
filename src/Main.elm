module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Debug
import Dict exposing (Dict)
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
import WebGL exposing (..)
import WebGL.Texture as Texture exposing (Error, Options, Texture, linear, nearest, repeat)



-- Application Model (textures)


type alias TextureContainer =
    Dict String Texture


type alias Model =
    { textures : Maybe TextureContainer
    , meshBank : MeshBank
    , enemyModel : Enemy.Model
    , playerModel : Player.Model
    }



-- Application Action (message in Elm), define action that can be performed by the runtime "asynchronously"


type Action
    = TexturesError Error
    | TexturesLoaded TextureContainer
    | AnimationFrame Float
    | GetViewport Viewport
    | OnKeyDown String
    | OnKeyUp String


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
            Dict.fromList [ ( "Alien", "../textures/shmup/shmup/color/alien1.png" ), ( "Player", "../textures/shmup/shmup/color/alien15.png" ) ]
    in
    ( { textures = Nothing
      , meshBank = meshBank
      , enemyModel = Enemy.init initMeshBank.textureMesh orthographicCamera
      , playerModel = Player.init initMeshBank.textureMesh orthographicCamera |> Player.withPosition (vec2 400 400)
      }
    , Cmd.batch
        [ loadTextures textureOptions textures
        , Task.perform GetViewport getViewport
        ]
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
            Debug.log "texture loading error : " err
                |> (\_ -> ( model, Cmd.none ))

        AnimationFrame delta ->
            let
                newModel =
                    { model
                        | enemyModel = Enemy.update model.enemyModel
                        , playerModel = Player.update model.playerModel
                    }
            in
            ( newModel, Cmd.none )

        GetViewport { viewport } ->
            let
                _ =
                    Debug.log "viewport :" viewport
            in
            ( model, Cmd.none )

        OnKeyDown keycode ->
            ( { model | playerModel = Player.handleKeyDown model.playerModel keycode }, Cmd.none )

        OnKeyUp keycode ->
            ( { model | playerModel = Player.handleKeyUp model.playerModel keycode }, Cmd.none )



-- Draw stuff here


view : Model -> Html msg
view model =
    case model.textures of
        -- textures not loaded
        Nothing ->
            text "Loading ..."

        Just textures ->
            WebGL.toHtmlWith
                [ WebGL.alpha True, WebGL.antialias ]
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
        , onKeyDown (keycodeDecoder OnKeyDown)
        , onKeyUp (keycodeDecoder OnKeyUp)
        ]


keycodeDecoder : (String -> Action) -> Decode.Decoder Action
keycodeDecoder action =
    Decode.map action (Decode.field "key" Decode.string)


orthographicCamera : Mat4
orthographicCamera =
    Mat4.makeOrtho2D 0 800 0 800



-- Tell elm runtime to perform tasks that load textures
-- @Todo move in submodule


loadTextures : Options -> Dict String String -> Cmd Action
loadTextures options texturesList =
    Dict.toList texturesList
        |> List.map (\( name, path ) -> Texture.loadWith options path |> Task.map (\texture -> ( name, texture )))
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
                        TexturesLoaded (Dict.fromList textures)
            )
