module Enemy exposing (..)

import Bullet exposing (..)
import Entity exposing (..)
import Gun exposing (..)
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
    , speed : Float -- Down speed
    , gun : Gun.Model
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        entity =
            Entity.empty mesh camera
                |> Entity.withSize (vec2 32 32)
    in
    Model entity -2.0 Gun.Unarmed


withPosition : Position -> Model -> Model
withPosition position model =
    { model | entity = Entity.withPosition position model.entity }


withSpeed : Float -> Model -> Model
withSpeed speed model =
    { model | speed = speed }


withTexture : String -> TextureContainer -> Model -> Model
withTexture textures textureName model =
    { model | entity = Entity.withTexture textures textureName model.entity }


withGun : Gun.Model -> Model -> Model
withGun gun model =
    { model | gun = gun }


update : Model -> Model
update ({ gun } as model) =
    { model | gun = Gun.updateBullets gun }
        |> goDown
        |> rotate
        |> shoot


goDown : Model -> Model
goDown ({ entity } as model) =
    { model | entity = Entity.applyVelocity (vec2 0 model.speed) entity }


rotate : Model -> Model
rotate ({ entity } as model) =
    { model | entity = Entity.withAngle (entity.renderingProperties.angle + 0.5) entity }


shoot : Model -> Model
shoot ({ gun } as model) =
    case gun of
        Gun.Unarmed ->
            model

        Gun.Armed g ->
            if g.bulletTimeout == 0 then
                { model
                    | gun =
                        g.bulletPrototype
                            |> Bullet.withPosition model.entity.renderingProperties.position
                            |> Gun.addBullet gun
                            |> Gun.withBulletTimeout 60
                }

            else
                model


view : Model -> List WebGL.Entity
view model =
    List.append (Entity.view model.entity) (Gun.view model.gun)
