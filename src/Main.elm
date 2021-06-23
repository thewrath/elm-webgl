module Main exposing (Uniforms, Vertex, fragmentShader, main, mesh, vertexShader, view)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


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
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { perspective = Mat4.makeOrtho2D 0 800 0 800 }
        ]



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 1 1)
          , Vertex (vec3 50 0 0) (vec3 1 1 1)
          , Vertex (vec3 50 50 0) (vec3 1 1 1)
          )
        , ( Vertex (vec3 0 0 0) (vec3 1 1 1)
          , Vertex (vec3 0 50 0) (vec3 1 1 1)
          , Vertex (vec3 50 50 0) (vec3 1 1 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color * vec3(1, 0, 0);
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
