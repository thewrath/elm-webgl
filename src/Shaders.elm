module Shaders exposing
    ( Uniforms
    , fragmentShader
    , texturedFragmentShader
    , vertexShader
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Type exposing (..)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)


vertexShader : Shader Vertex { perspective : Mat4 } { vcolor : Vec3 }
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


fragmentShader : Shader {} { perspective : Mat4 } { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


texturedFragmentShader : Shader {} { perspective : Mat4, texture : Texture } { vcolor : vec3 }
texturedFragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }
    |]
