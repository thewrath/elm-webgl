module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Bullet exposing (..)
import Collision exposing (handleManyToMany)
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
      , playerModel = Player.init meshBank.textureMesh Constant.orthographicCamera |> Player.withPosition (vec2 (Constant.getWidth / 2) (Constant.getHeight / 2))
      }
    , Cmd.batch
        [ loadTextures textureOptions textures TexturesLoaded TexturesError
        , Task.perform GetViewport getViewport
        ]
    )



-- Update function, handle message and bring your entity to life


update : Action -> Model -> ( Model, Cmd Action )
update action ({ wave, playerModel, meshBank } as model) =
    case action of
        TexturesLoaded textures ->
            let
                enemyGun =
                    Gun.empty
                        |> Gun.withBulletPrototype
                            (Bullet.init meshBank.textureMesh Constant.orthographicCamera
                                |> Bullet.withTexture "Bullet" textures
                                |> Bullet.withSpeed (-Constant.getDefaultBulletSpeed / 2)
                            )

                waveEnemyPrototype =
                    Wave.createEnemyPrototype meshBank.textureMesh textures |> Enemy.withGun enemyGun
            in
            ( { model
                | textures = Just textures
                , wave = Wave.withEnemyPrototype waveEnemyPrototype wave
                , playerModel = Player.onTexturesLoaded textures playerModel
              }
            , Cmd.none
            )

        TexturesError err ->
            Debug.log "texture loading error : " err
                |> (\_ -> ( model, Cmd.none ))

        AnimationFrame delta ->
            let
                oldGun =
                    playerModel.gun

                newModel =
                    model
                        |> updateWave
                            (wave
                                |> Wave.withEnemies (handleManyToMany wave.enemies .entity (Gun.getBullets oldGun) .entity)
                                |> Wave.update
                            )
                        |> updatePlayerModel
                            (Player.update playerModel wave.enemies
                                |> Player.handleBulletsCollision (Wave.getBullets wave)
                            )
            in
            ( newModel, Cmd.none )

        GetViewport { viewport } ->
            ( model, Cmd.none )

        OnKeyDown keycode ->
            Cmd.none
                |> Tuple.pair (updatePlayerModel (Player.withKeyStates (KeyHandler.handleKeyDown playerModel.keyStates keycode) playerModel) model)

        OnKeyUp keycode ->
            Cmd.none
                |> Tuple.pair (updatePlayerModel (Player.withKeyStates (KeyHandler.handleKeyUp playerModel.keyStates keycode) playerModel) model)


updatePlayerModel : Player.Model -> Model -> Model
updatePlayerModel playerModel model =
    { model | playerModel = playerModel }


updateWave : Wave.Model -> Model -> Model
updateWave wave model =
    { model | wave = wave }


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
                , style "border-color" "blue"
                , style "border-style" "solid"
                , style "background-color" "#0F0B3C"
                , style "margin" "auto"
                ]
                (List.concat [ Wave.view wave, Player.view playerModel ])


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\f -> AnimationFrame f)
        , onKeyDown (KeyHandler.keycodeDecoder OnKeyDown)
        , onKeyUp (KeyHandler.keycodeDecoder OnKeyUp)
        ]
