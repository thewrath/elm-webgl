module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Bullet exposing (..)
import Collision exposing (..)
import Constant exposing (..)
import Dict exposing (Dict)
import Enemy exposing (..)
import Entity exposing (..)
import Gun exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import KeyHandler exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, add, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Player exposing (..)
import Render exposing (..)
import RenderingProperties exposing (..)
import Shaders exposing (..)
import Task exposing (..)
import Texture exposing (TextureContainer, loadTextures)
import Tuple exposing (..)
import Type exposing (..)
import Wave exposing (..)
import WebGL exposing (..)
import WebGL.Texture exposing (Error, Options, linear, nearest, repeat)



-- Application Model (textures)


type alias Model =
    { textures : Maybe TextureContainer
    , meshBank : MeshBank
    , wave : Wave.Model
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
            Dict.fromList
                [ ( "Alien", "../textures/shmup/shmup/color/alien1.png" )
                , ( "Player", "../textures/shmup/shmup/color/alien15.png" )
                , ( "Bullet", "../textures/shmup/shmup/color/alien22.png" )
                ]
    in
    ( { textures = Nothing
      , meshBank = meshBank
      , wave = Wave.empty
      , playerModel = Player.init initMeshBank.textureMesh Constant.orthographicCamera |> Player.withPosition (vec2 400 400)
      }
    , Cmd.batch
        [ loadTextures textureOptions textures TexturesLoaded TexturesError
        , Task.perform GetViewport getViewport
        ]
    )



-- Update function, handle message and bring your entity to life


update : Action -> Model -> ( Model, Cmd Action )
update action ({ wave, playerModel } as model) =
    case action of
        TexturesLoaded textures ->
            ( { model
                | textures = Just textures
                , wave = Wave.withEnemyPrototype (Wave.createEnemyPrototype initMeshBank.textureMesh textures) wave
                , playerModel = Player.onTexturesLoaded textures playerModel
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
                        | wave = Wave.update wave
                        , playerModel = Player.update playerModel
                    }
            in
            ( newModel, Cmd.none )

        GetViewport { viewport } ->
            ( model, Cmd.none )

        OnKeyDown keycode ->
            Cmd.none
                |> Tuple.pair (updatePlayerModel model (Player.withKeyStates (KeyHandler.handleKeyDown playerModel.keyStates keycode) playerModel))

        OnKeyUp keycode ->
            Cmd.none
                |> Tuple.pair (updatePlayerModel model (Player.withKeyStates (KeyHandler.handleKeyUp playerModel.keyStates keycode) playerModel))


updatePlayerModel : Model -> Player.Model -> Model
updatePlayerModel model playerModel =
    { model | playerModel = playerModel }



-- @Todo Can be easily abstracted into "check for collisions between two List Entity".


checkEnemiesBulletsCollision : List Enemy.Model -> List Bullet.Model -> Bool
checkEnemiesBulletsCollision enemies bullets =
    let
        checkEnemyBulletsCollision enemy =
            (not << List.isEmpty) <| List.filter (\b -> Collision.checkCollision (Entity.toCollisionBox b.entity) (Entity.toCollisionBox enemy.entity)) bullets
    in
    List.foldl ((||) << checkEnemyBulletsCollision) False enemies


view : Model -> Html msg
view ({ playerModel, wave } as model) =
    case model.textures of
        -- textures not loaded
        Nothing ->
            text "Loading ..."

        Just textures ->
            WebGL.toHtmlWith
                [ WebGL.alpha True, WebGL.antialias ]
                [ width Constant.getWidth
                , height Constant.getHeight
                , style "display" "block"
                , style "background-color" "black"
                , style "margin" "auto"
                ]
                (List.concat [ Gun.view playerModel.gun, Wave.view wave, Entity.view playerModel.entity ])


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\f -> AnimationFrame f)
        , onKeyDown (KeyHandler.keycodeDecoder OnKeyDown)
        , onKeyUp (KeyHandler.keycodeDecoder OnKeyUp)
        ]
