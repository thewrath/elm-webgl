module KeyHandler exposing (..)

import Dict exposing (..)


type alias KeyStates =
    Dict String Bool


handleKeyDown : KeyStates -> String -> KeyStates
handleKeyDown keyStates keycode =
    Dict.insert keycode True keyStates


handleKeyUp : KeyStates -> String -> KeyStates
handleKeyUp keyStates keycode =
    Dict.insert keycode False keyStates
