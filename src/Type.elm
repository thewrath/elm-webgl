module Type exposing (..)

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
