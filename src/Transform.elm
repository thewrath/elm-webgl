module Transform exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4, identity, transform, translate3)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)



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
