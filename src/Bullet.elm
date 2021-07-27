module Bullet exposing (..)

import Entity exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { entity : Entity.Model
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    { entity = Entity.empty mesh camera }


withTexture : String -> TextureContainer -> Model -> Model
withTexture textureName textures model =
    { model | entity = Entity.withTexture textureName textures model.entity }


clone : Model -> Model
clone model =
    model


update : Model -> Model
update ({ entity } as model) =
    { model | entity = updateEntity entity }


updateEntity : Entity.Model -> Entity.Model
updateEntity entity =
    entity
