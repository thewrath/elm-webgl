module Wave exposing (..)

import Constant exposing (..)
import Enemy exposing (..)
import Entity exposing (..)
import Math.Vector2 as Vec2 exposing (getY, vec2)
import Texture exposing (..)
import Type exposing (..)
import WebGL exposing (Entity, Mesh)


type alias Model =
    { enemyPrototype : Maybe Enemy.Model
    , enemies : List Enemy.Model
    , timeout : Int
    }


empty : Model
empty =
    Model Nothing [] getTimeout


withEnemyPrototype : Enemy.Model -> Model -> Model
withEnemyPrototype enemyPrototype model =
    { model | enemyPrototype = Just enemyPrototype }


createEnemyPrototype : Mesh TextureVertex -> TextureContainer -> Enemy.Model
createEnemyPrototype mesh textures =
    Enemy.init mesh Constant.orthographicCamera
        |> Enemy.withTexture "Alien" textures
        |> Enemy.withPosition (vec2 400 825)


addEnemy : Model -> Model
addEnemy ({ enemies, enemyPrototype } as model) =
    case enemyPrototype of
        Nothing ->
            model

        Just enemy ->
            { model | enemies = enemy :: enemies }


update : Model -> Model
update ({ timeout, enemies } as model) =
    let
        _ =
            Debug.log "enemies" (List.length enemies)

        onTimeout fn m =
            if timeout == 0 then
                { m | timeout = getTimeout }
                    |> fn

            else
                { m | timeout = timeout - 1 }
    in
    { model | enemies = List.map Enemy.update enemies }
        |> destroyOutOfScreenEnemies
        |> onTimeout addEnemy


destroyOutOfScreenEnemies : Model -> Model
destroyOutOfScreenEnemies ({ enemies } as model) =
    { model | enemies = List.filter (not << isOutOfScreen) enemies }



-- Only check y axe because bullet only go up


isOutOfScreen : Enemy.Model -> Bool
isOutOfScreen enemy =
    getY enemy.entity.renderingProperties.position < 0


view : Model -> List WebGL.Entity
view model =
    model.enemies |> List.map (.entity >> Entity.view) |> List.concat


getTimeout =
    60 * 3
