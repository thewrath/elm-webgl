module Gun exposing (..)

import Bullet exposing (..)
import Collision exposing (..)
import Constant exposing (..)
import Enemy exposing (..)
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


withBulletPrototype : Bullet.Model -> Model -> Model
withBulletPrototype bulletPrototype model =
    Armed { bullets = [], bulletPrototype = bulletPrototype, bulletTimeout = 0 }


ifArmed : Model -> (Gun -> Model) -> Model
ifArmed model fn =
    case model of
        Unarmed ->
            model

        Armed g ->
            fn g


withBulletTimeout : Float -> Model -> Model
withBulletTimeout timeout model =
    ifArmed model (\g -> Armed { g | bulletTimeout = timeout })


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


handleEnemiesCollision : List Enemy.Model -> Model -> Model
handleEnemiesCollision enemies model =
    case model of
        Unarmed ->
            model

        Armed gun ->
            let
                checkBulletEnemiesCollision bullet =
                    (not << List.isEmpty) <| List.filter (\e -> Collision.checkCollision (Entity.toCollisionBox bullet.entity) (Entity.toCollisionBox e.entity)) enemies

                bullets =
                    List.filter (not << checkBulletEnemiesCollision) gun.bullets
            in
            Armed { gun | bullets = bullets }



-- Only check y axe because bullet only go up


isOutOfScreen : Bullet.Model -> Bool
isOutOfScreen bullet =
    getY bullet.entity.renderingProperties.position > Constant.getHeight


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
