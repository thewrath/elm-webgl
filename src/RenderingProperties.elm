module RenderingProperties exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Type exposing (..)


type alias RenderingProperties =
    { position : Position
    , size : Size
    , angle : Float
    }



-- Create empty RenderingProperties records


empty : RenderingProperties
empty =
    { position = vec2 0 0, size = vec2 0 0, angle = 0 }


withPosition : Position -> RenderingProperties -> RenderingProperties
withPosition position renderingProperties =
    { renderingProperties | position = position }


withSize : Size -> RenderingProperties -> RenderingProperties
withSize size renderingProperties =
    { renderingProperties | size = size }


withAngle : Float -> RenderingProperties -> RenderingProperties
withAngle angle renderingProperties =
    { renderingProperties | angle = angle }
