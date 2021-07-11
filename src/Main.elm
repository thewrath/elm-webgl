module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, text)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render exposing (renderSprite, renderSquare)
import Shaders exposing (..)
import Task exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)



-- Application Model (textures)


type alias Model =
    { textures : Maybe (List Texture)
    }



-- Application Action (message in Elm), define action that can be performed by the runtime "asynchronously"


type Action
    = TexturesError Error
    | TexturesLoaded (List Texture)


main : Program Value Model Action
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }



-- Initialize application (fetch textures, ...)


init : ( Model, Cmd Action )
init =
    let
        textures =
            [ "../textures/thwomp-face.jpg", "../textures/thwomp-side.jpg" ]
    in
    ( { textures = Nothing }, Cmd.batch [ loadTextures textures ] )



-- Update function, handle message and bring your entity to life


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TexturesLoaded textures ->
            ( { model | textures = Just textures }, Cmd.none )

        TexturesError err ->
            ( model, Cmd.none )



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
                    WebGL.toHtml
                        [ width 800
                        , height 800
                        , style "display" "block"
                        , style "background-color" "black"
                        , style "margin" "auto"
                        ]
                        [ renderSprite 50 50 texture orthographicCamera
                        ]


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
