module Bullet exposing (..)

import Type exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture exposing (Texture)


type alias Model =
    { texture : Maybe WebGL.Texture.Texture
    }


init :
    Model
        { texture : Nothing }


update : Model -> Model
update model =
    model
