module Player exposing (..)

import Bullet exposing (..)
import Collision exposing (handleOneToMany)
import Constant exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Enemy exposing (..)
import Entity exposing (..)
import Gun exposing (..)
import Json.Decode as Decode
import KeyHandler exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, length, vec2)
import RenderingProperties exposing (..)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { entity : Entity.Model
    , speed : Float -- Speed in all direction
    , keyStates : KeyStates
    , gun : Gun.Model
    , isMoving : Bool
    , isShooting : Bool
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
    { entity = entity
    , speed = Constant.getPlayerSpeed
    , keyStates = Dict.empty
    , gun = Gun.Unarmed
    , isMoving = False
    , isShooting = False
    }


withPosition : Position -> Model -> Model
withPosition position model =
    { model | entity = Entity.withPosition position model.entity }


withKeyStates : KeyStates -> Model -> Model
withKeyStates keyStates model =
    { model | keyStates = keyStates }


onTexturesLoaded : TextureContainer -> Model -> Model
onTexturesLoaded textureContainer ({ gun, entity } as model) =
    { model
        | gun = gun |> Gun.withBulletPrototype (Bullet.init model.entity.mesh model.entity.camera |> Bullet.withTexture "Bullet" textureContainer)
        , entity = entity |> Entity.withTexture "Player" textureContainer
    }


update : Model -> List Enemy.Model -> Model
update model enemies =
    model
        |> move
        |> shoot
        |> updateGun enemies


updateGun : List Enemy.Model -> Model -> Model
updateGun enemies ({ gun } as model) =
    { model
        | gun =
            gun
                |> Gun.withBullets (Collision.handleManyToMany (Gun.getBullets gun) .entity enemies .entity)
                |> Gun.updateBullets
    }



-- Move player depending on keyStates


move : Model -> Model
move ({ entity } as model) =
    let
        keyActions =
            Dict.fromList
                [ ( "ArrowUp", vec2 0 1 )
                , ( "ArrowRight", vec2 1 0 )
                , ( "ArrowDown", vec2 0 -1 )
                , ( "ArrowLeft", vec2 -1 0 )
                ]
    in
    Dict.keys keyActions
        |> List.map
            (\key ->
                Maybe.withDefault False (Dict.get key model.keyStates)
                    |> (\keyState ->
                            if keyState == True then
                                Maybe.withDefault (vec2 0 0) (Dict.get key keyActions)

                            else
                                vec2 0 0
                       )
            )
        |> List.foldr Vec2.add (vec2 0 0)
        |> Vec2.scale model.speed
        -- @Todo Add correction of higher speed in diagonal direction
        |> applyVelocity model
        -- Up side
        |> checkSide (\p s -> ( p.y + s.y >= Constant.getHeight, vec2 p.x (Constant.getHeight - s.y) ))
        -- Right side
        |> checkSide (\p s -> ( p.x + s.x >= Constant.getWidth, vec2 (Constant.getWidth - s.x) p.y ))
        -- Down side
        |> checkSide (\p s -> ( p.y - s.y <= 0, vec2 p.x s.y ))
        -- Left side
        |> checkSide (\p s -> ( p.x - s.x <= 0, vec2 s.x p.y ))


applyVelocity : Model -> Vec2 -> Model
applyVelocity model velocity =
    let
        isMoving =
            not <| (Vec2.length velocity == 0)
    in
    { model | entity = Entity.applyVelocity velocity model.entity, isMoving = isMoving }


checkSide : ({ x : Float, y : Float } -> { x : Float, y : Float } -> ( Bool, Vec2 )) -> Model -> Model
checkSide checker ({ entity } as model) =
    let
        position =
            entity.renderingProperties.position

        size =
            entity.renderingProperties.size

        ( collision, newPosition ) =
            checker (Vec2.toRecord position) (Vec2.toRecord size)
    in
    if collision then
        withPosition newPosition model

    else
        model



-- @Todo reduce player lives


handleBulletsCollision : List Bullet.Model -> Model -> Model
handleBulletsCollision bullets model =
    if handleOneToMany model .entity bullets .entity then
        model

    else
        model



-- Shoot bullet on space down


shoot : Model -> Model
shoot ({ gun } as model) =
    case gun of
        Unarmed ->
            model

        Armed g ->
            -- check if space is down
            if Maybe.withDefault False (Dict.get " " model.keyStates) && g.bulletTimeout == 0 then
                { model
                    | gun =
                        g.bulletPrototype
                            |> Bullet.withPosition model.entity.renderingProperties.position
                            |> Gun.addBullet gun
                            |> Gun.withBulletTimeout 12
                    , isShooting = True
                }

            else if g.bulletTimeout /= 0 then
                { model | isShooting = model.isShooting }

            else
                { model | isShooting = False }


getBullets : Model -> List Bullet.Model
getBullets model =
    case model.gun of
        Unarmed ->
            []

        Armed gun ->
            gun.bullets


view : Model -> List WebGL.Entity
view model =
    List.append (Entity.view model.entity) (Gun.view model.gun)
