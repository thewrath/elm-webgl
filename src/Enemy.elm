module Enemy exposing (..)

import Entity exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { entity : Entity.Model
    , speed : Float -- Down speed
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        entity =
            Entity.empty mesh camera
                |> Entity.withPosition (vec2 0 0)
                |> Entity.withSize (vec2 32 32)
                |> Entity.withAngle 0
    in
    Model entity -2.0


withPosition : Position -> Model -> Model
withPosition position model =
    { model | entity = Entity.withPosition position model.entity }


withSpeed : Float -> Model -> Model
withSpeed speed model =
    { model | speed = speed }


update : Model -> Model
update model =
    model
        |> goDown


goDown : Model -> Model
goDown ({ entity } as model) =
    { model | entity = Entity.applyVelocity (vec2 0 model.speed) entity }
