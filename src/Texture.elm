module Texture exposing (..)

import Dict exposing (Dict)
import Task exposing (..)
import WebGL.Texture exposing (Error, Options, Texture)


type alias TextureContainer =
    Dict String Texture


loadTextures : Options -> Dict String String -> (TextureContainer -> msg) -> (Error -> msg) -> Cmd msg
loadTextures options texturesList onSuccess onError =
    Dict.toList texturesList
        |> List.map (\( name, path ) -> WebGL.Texture.loadWith options path |> Task.map (\texture -> ( name, texture )))
        -- turn textures into list of "loading texture task"
        |> Task.sequence
        -- create a new sequence task (task compose of a list of tasks)
        |> Task.andThen Task.succeed
        -- Turn task result into message that can be handle in update function
        |> Task.attempt
            (\result ->
                case result of
                    Err error ->
                        onError error

                    Ok textures ->
                        onSuccess (Dict.fromList textures)
            )
