module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render exposing (renderSprite)
import Shaders exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)


main : Program Value Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 800
        , height 800
        , style "display" "block"
        , style "background-color" "black"
        , style "margin" "auto"
        ]
        [ renderSprite 50 50 "test" (Mat4.makeOrtho2D 0 800 0 800)
        , renderSprite 250 50 "test" (Mat4.makeOrtho2D 0 800 0 800)
        ]
