module Type exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias ColoredVertex =
    { position : Vec3
    , color : Vec3
    }


type alias TextureVertex =
    { position : Vec3
    , coord : Vec2
    }


type alias RenderingProperties =
    { position : Position
    , size : Size
    , angle : Float
    }


type alias Position =
    Vec2


type alias Size =
    Vec2


type alias Camera =
    Mat4
