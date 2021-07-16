module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, add, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render exposing (MeshBank, initMeshBank, renderSprite, renderSquare)
import Shaders exposing (..)
import Task exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)



-- Application Model (textures)


type alias Sprite =
    { position : Vec2
    , angle : Float
    }


type alias Model =
    { textures : Maybe (List Texture)
    , meshBank : MeshBank
    , sprite : Sprite
    }



-- Application Action (message in Elm), define action that can be performed by the runtime "asynchronously"


type Action
    = TexturesError Error
    | TexturesLoaded (List Texture)
    | AnimationFrame Float


main : Program Value Model Action
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta (\f -> AnimationFrame f)
        , update = update
        }



-- Initialize application (fetch textures, ...)


init : ( Model, Cmd Action )
init =
    let
        textures =
            [ "../textures/thwomp-face.jpg", "../textures/thwomp-side.jpg" ]

        sprite =
            { position = vec2 150 150, angle = 0 }
    in
    ( { textures = Nothing, meshBank = initMeshBank, sprite = sprite }, Cmd.batch [ loadTextures textures ] )



-- Update function, handle message and bring your entity to life


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TexturesLoaded textures ->
            ( { model | textures = Just textures }, Cmd.none )

        TexturesError err ->
            ( model, Cmd.none )

        AnimationFrame delta ->
            ( updateSprite model, Cmd.none )


updateSprite : Model -> Model
updateSprite model =
    let
        sprite =
            { position = model.sprite.position, angle = model.sprite.angle + 0.05 } |> Debug.log "sprite"
    in
    { model | sprite = sprite }



-- Draw stuff here


view : Model -> Html msg
view model =
    case model.textures of
        -- textures not loaded
        Nothing ->
            text "Loading ..."

        Just textures ->
            let
                firstTexture =
                    List.head textures
            in
            case firstTexture of
                Nothing ->
                    text "Texture loading error"

                Just texture ->
                    let
                        _ =
                            Debug.log "size :" (Texture.size texture)

                        textureMesh =
                            model.meshBank.textureMesh

                        coloredMesh =
                            model.meshBank.coloredMesh

                        defaultRenderingProperties =
                            { position = vec2 50 50
                            , size = vec2 50 50
                            , angle = 10
                            }

                        spriteRenderingProperties =
                            { position = model.sprite.position
                            , size = vec2 50 50
                            , angle = model.sprite.angle
                            }
                    in
                    WebGL.toHtml
                        [ width 800
                        , height 800
                        , style "displaMeshy" "block"
                        , style "background-color" "black"
                        , style "margin" "auto"
                        ]
                        (List.repeat 100 spriteRenderingProperties
                            |> List.indexedMap (\i rp -> updatePosition i rp)
                            |> List.indexedMap (\i rp -> updateAngle i rp)
                            |> List.map (\rp -> renderSprite textureMesh rp texture orthographicCamera)
                        )


updatePosition : Int -> RenderingProperties -> RenderingProperties
updatePosition index renderingProperties =
    if modBy 2 index == 0 then
        { renderingProperties | position = add renderingProperties.position (vec2 (20 * toFloat index) 0) }

    else
        { renderingProperties | position = add renderingProperties.position (vec2 0 (20 * toFloat index)) }


updateAngle : Int -> RenderingProperties -> RenderingProperties
updateAngle index renderingProperties =
    if modBy 2 index == 0 then
        { renderingProperties | angle = negate renderingProperties.angle }

    else
        renderingProperties


orthographicCamera : Mat4
orthographicCamera =
    Mat4.makeOrtho2D 0 800 0 800



-- Tell elm runtime to perform tasks that load textures
-- @Todo move in submodule


loadTextures : List String -> Cmd Action
loadTextures textureNames =
    textureNames
        |> List.map Texture.load
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
