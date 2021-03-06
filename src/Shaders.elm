module Shaders exposing
    ( fragmentShader
    , texturedFragmentShader
    , texturedVertexShader
    , vertexShader
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Type exposing (..)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)


vertexShader : Shader ColoredVertex { perspective : Mat4, transform : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * transform * vec4(position, 1.0);
            vcolor = color * vec3(1, 0, 0);
        }
    |]


fragmentShader : Shader {} { perspective : Mat4, transform : Mat4 } { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


texturedVertexShader : Shader TextureVertex { perspective : Mat4, transform : Mat4, texture : Texture } { vcoord : Vec2 }
texturedVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec2 vcoord;
        void main () {
          gl_Position = perspective * transform * vec4(position, 1.0);
          vcoord = coord.xy;
        }
    |]


texturedFragmentShader : Shader {} { perspective : Mat4, transform : Mat4, texture : Texture } { vcoord : Vec2 }
texturedFragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }
    |]
