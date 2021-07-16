module Transform exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Type exposing (..)



-- Apply transform to triangle


transformTriangle : Mat4 -> (Mat4 -> a -> a) -> ( a, a, a ) -> ( a, a, a )
transformTriangle transformMat transformVertex_ triangles =
    let
        transformTriangle_ ( a, b, c ) =
            ( transformVertex_ transformMat a
            , transformVertex_ transformMat b
            , transformVertex_ transformMat c
            )
    in
    transformTriangle_ triangles



-- Apply transformation to a vertex


transformVertex : Mat4 -> { a | position : Vec3 } -> { a | position : Vec3 }
transformVertex transformMat vertex =
    { vertex | position = Mat4.transform transformMat vertex.position }



-- Turn rendering properties into transformation matrix that can be used in shader.


transformRenderingProperties : RenderingProperties -> Mat4
transformRenderingProperties renderingProperties =
    Mat4.identity
        |> Mat4.translate (vec3 (getX renderingProperties.position) (getY renderingProperties.position) 1)
        |> Mat4.rotate renderingProperties.angle (vec3 0 0 1)
        |> Mat4.scale (vec3 (getX renderingProperties.size) (getY renderingProperties.size) 1)
