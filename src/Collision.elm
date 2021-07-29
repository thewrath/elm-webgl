module Collision exposing (..)

import Math.Vector2 as Vec2 exposing (getX, getY)
import Type exposing (..)


type alias Box =
    { position : Position, size : Size }


checkCollision : Box -> Box -> Bool
checkCollision boxA boxB =
    (getX boxA.position < getX boxB.position + getX boxB.size)
        && (getX boxA.position + getX boxA.size > getX boxB.position)
        && (getY boxA.position < getY boxB.position + getY boxB.size)
        && (getY boxA.size + getY boxA.position > getY boxB.position)
