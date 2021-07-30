module Collision exposing (..)

import Entity exposing (..)
import Math.Vector2 as Vec2 exposing (getX, getY)
import Type exposing (..)


type alias Box =
    { position : Position, size : Size }


boxFromEntity : Entity.Model -> Box
boxFromEntity model =
    Box model.renderingProperties.position model.renderingProperties.size


checkCollision : Box -> Box -> Bool
checkCollision boxA boxB =
    (getX boxA.position < getX boxB.position + getX boxB.size)
        && (getX boxA.position + getX boxA.size > getX boxB.position)
        && (getY boxA.position < getY boxB.position + getY boxB.size)
        && (getY boxA.size + getY boxA.position > getY boxB.position)


handleListCollision : List a -> (a -> Entity.Model) -> List b -> (b -> Entity.Model) -> List a
handleListCollision targets targetTransform against againstTransform =
    let
        checkOneToOther t =
            (not << List.isEmpty) <| List.filter (\a -> checkCollision (targetTransform t |> boxFromEntity) (againstTransform a |> boxFromEntity)) against
    in
    List.filter (not << checkOneToOther) targets
