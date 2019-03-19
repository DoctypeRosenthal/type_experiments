module Main exposing (main)

import Browser
import Rule



---- PROGRAM ----


main : Program () Rule.Model Rule.Msg
main =
    Browser.element
        { view = Rule.view
        , init = \_ -> Rule.init
        , update = Rule.update
        , subscriptions = always Sub.none
        }
