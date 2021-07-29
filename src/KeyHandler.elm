module KeyHandler exposing (..)

import Dict exposing (..)
import Json.Decode as Decode exposing (Value)


type alias KeyStates =
    Dict String Bool


handleKeyDown : KeyStates -> String -> KeyStates
handleKeyDown keyStates keycode =
    Dict.insert keycode True keyStates


handleKeyUp : KeyStates -> String -> KeyStates
handleKeyUp keyStates keycode =
    Dict.insert keycode False keyStates


keycodeDecoder : (String -> msg) -> Decode.Decoder msg
keycodeDecoder action =
    Decode.map action (Decode.field "key" Decode.string)
