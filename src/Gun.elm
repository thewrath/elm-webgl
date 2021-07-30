module Gun exposing (..)

import Bullet exposing (..)
import Constant exposing (..)
import Entity exposing (..)
import Math.Vector2 as Vec2 exposing (getY)
import WebGL exposing (Entity, Mesh, Shader)


type alias Gun =
    { bullets : List Bullet.Model
    , bulletPrototype : Bullet.Model
    , bulletTimeout : Float
    }


type Model
    = Unarmed
    | Armed Gun


empty : Model
empty =
    Unarmed


withBulletPrototype : Bullet.Model -> Model -> Model
withBulletPrototype bulletPrototype model =
    Armed { bullets = [], bulletPrototype = bulletPrototype, bulletTimeout = 0 }


withBulletTimeout : Float -> Model -> Model
withBulletTimeout timeout model =
    ifArmed model (\g -> Armed { g | bulletTimeout = timeout })


withBullets : List Bullet.Model -> Model -> Model
withBullets bullets model =
    case model of
        Unarmed ->
            model

        Armed gun ->
            Armed { gun | bullets = bullets }


ifArmed : Model -> (Gun -> Model) -> Model
ifArmed model fn =
    case model of
        Unarmed ->
            model

        Armed g ->
            fn g


addBullet : Model -> Bullet.Model -> Model
addBullet model bullet =
    ifArmed model (\g -> Armed { g | bullets = bullet :: g.bullets })


updateBullets : Model -> Model
updateBullets model =
    ifArmed model
        (\g ->
            Armed
                ({ g | bullets = List.map Bullet.update g.bullets, bulletTimeout = clamp 0 100 (g.bulletTimeout - 1) }
                    |> destroyOutOfScreenBullets
                )
        )


destroyOutOfScreenBullets : Gun -> Gun
destroyOutOfScreenBullets ({ bullets } as gun) =
    { gun | bullets = List.filter (not << isOutOfScreen) bullets }


isOutOfScreen : Bullet.Model -> Bool
isOutOfScreen bullet =
    (getY bullet.entity.renderingProperties.position > Constant.getHeight)
        || (getY bullet.entity.renderingProperties.position < 0)


view : Model -> List WebGL.Entity
view gun =
    case gun of
        Unarmed ->
            []

        Armed ({ bullets } as model) ->
            List.concat (List.map (.entity >> Entity.view) bullets)


getBullets : Model -> List Bullet.Model
getBullets model =
    case model of
        Unarmed ->
            []

        Armed gun ->
            gun.bullets
