module Tests exposing (all)

import BaseTypes exposing (natNum)
import Expect
import Main exposing (Msg(..), init, update)
import Test exposing (..)
import Trigger exposing (Condition(..), defaultTrigger)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "Add default Trigger" <|
            \_ ->
                let
                    ( initModel, _ ) =
                        init

                    ( model, _ ) =
                        update AddTrigger initModel

                    res =
                        { triggers = [ Trigger.Clicks <| LessThan <| natNum 1 ] }
                in
                Expect.equal model res
        ]
