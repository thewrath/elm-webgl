-- @Todo : Share code with Player module


module Enemy exposing (..)

import Dict exposing (Dict)
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
    { mesh : Mesh TextureVertex
    , texture : Maybe WebGL.Texture.Texture
    , camera : Mat4
    , renderingProperties : RenderingProperties
    }


init : Mesh TextureVertex -> Mat4 -> Model
init mesh camera =
    let
        renderingProperties =
            RenderingProperties.empty
                |> RenderingProperties.withPosition (vec2 0 0)
                |> RenderingProperties.withSize (vec2 32 32)
                |> RenderingProperties.withAngle 0
    in
    { mesh = mesh
    , texture = Nothing
    , camera = camera
    , renderingProperties = renderingProperties
    }


toEntity : Model -> Entity.BaseModel
toEntity model =
    { texture = model.texture
    , mesh = model.mesh
    , camera = model.camera
    , renderingProperties = model.renderingProperties
    }


update : Model -> Model
update model =
    { model | renderingProperties = RenderingProperties.withAngle (model.renderingProperties.angle + 0.05) model.renderingProperties }
