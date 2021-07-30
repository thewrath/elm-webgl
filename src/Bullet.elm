module Bullet exposing (..)

import Collision exposing (..)
import Constant exposing (..)
import Entity exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, add, vec2)
import Render exposing (..)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { entity : Entity.Model
    , speed : Float
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        entity =
            Entity.empty mesh camera
                |> Entity.withPosition (vec2 -50 -50)
                |> Entity.withSize (vec2 8 8)
                |> Entity.withAngle 0
    in
    Model entity Constant.getDefaultBulletSpeed


withPosition : Position -> Model -> Model
withPosition position model =
    { model | entity = Entity.withPosition position model.entity }


withSpeed : Float -> Model -> Model
withSpeed speed model =
    { model | speed = speed }


withTexture : String -> TextureContainer -> Model -> Model
withTexture textureName textures model =
    { model | entity = Entity.withTexture textureName textures model.entity }


update : Model -> Model
update ({ entity, speed } as model) =
    { model | entity = Entity.applyVelocity (vec2 0 speed) entity }
