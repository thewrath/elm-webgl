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
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        entity =
            Entity.empty mesh camera
                |> Entity.withPosition (vec2 50 50)
                |> Entity.withSize (vec2 32 32)
                |> Entity.withAngle 0
    in
    { entity = entity }


update : Model -> Model
update ({ entity } as model) =
    { model | entity = Entity.withAngle (entity.renderingProperties.angle + 0.05) entity }
