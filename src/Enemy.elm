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
    { entity = Entity.init mesh camera }


update : Model -> Model
update ({ entity } as model) =
    { model | entity = updateEntity entity }


updateEntity : Entity.Model -> Entity.Model
updateEntity entity =
    { entity | renderingProperties = RenderingProperties.withAngle (entity.renderingProperties.angle + 0.05) entity.renderingProperties }
