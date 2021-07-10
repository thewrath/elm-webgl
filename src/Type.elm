module Type exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }
