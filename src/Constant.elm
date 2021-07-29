module Constant exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)


orthographicCamera : Mat4
orthographicCamera =
    Mat4.makeOrtho2D 0 getWidth 0 getHeight


getWidth =
    800


getHeight =
    800


getWaveDefaultTimeout =
    60 * 3


getScoreUnit =
    10
