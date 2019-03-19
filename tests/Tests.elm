module Tests exposing (all)

import BaseTypes exposing (natNum, percent)
import Currency
import Expect
import Json.Decode
import NatNumCondition
import PercentCondition
import Rule exposing (Msg(..), init, update)
import Test exposing (..)
import Trigger exposing (defaultTrigger)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


json =
    """{
    "country": "DE",
    "monitor_type": "campaigns",
    "condition_matching": 1,
    "triggers": [
        {
          "condition_key": "greater_than",
          "field": "acos",
          "value_type" : "",
          "value": "15"
        },
        {
          "condition_key": "less_than",
          "value_type" : "",
          "field": "acos",
          "value": "25"
        },
        {
          "condition_key": "greater_than",
          "value_type" : "",
          "field": "clicks",
          "value": "9"
        }
    ]
    }
    """


jsonResult =
    { country = "DE"
    , monitor_type = "campaigns"
    , condition_matching = 1
    , triggers =
        [ { condition_key = "greater_than"
          , field = "acos"
          , value_type = ""
          , value = "15"
          }
        , { condition_key = "less_than"
          , field = "acos"
          , value_type = ""
          , value = "25"
          }
        , { condition_key = "greater_than"
          , field = "clicks"
          , value_type = ""
          , value = "9"
          }
        ]
    }


transformed =
    { currency = Currency.euro
    , monitor = Rule.MonitorCampaigns
    , triggerLogic = Trigger.And
    , triggers =
        [ Trigger.ACoS <| PercentCondition.GreaterThan <| percent 15
        , Trigger.ACoS <| PercentCondition.LessThan <| percent 25
        , Trigger.Clicks <| NatNumCondition.GreaterThan <| natNum 9
        ]
    }


all : Test
all =
    describe "A Test Suite"
        [ test "Add default Trigger" <|
            \_ ->
                let
                    ( initModel, _ ) =
                        init

                    ( model, _ ) =
                        update Rule.AddTrigger initModel

                    res =
                        { triggers = [ Trigger.Clicks <| NatNumCondition.LessThan <| natNum 0 ] }
                in
                Expect.equal model.triggers res.triggers
        , test "Parse JSON" <|
            \_ ->
                let
                    res =
                        Json.Decode.decodeString Rule.decoder json
                in
                Expect.equal res (Ok jsonResult)
        , test "Transform parsed JSON to triggers" <|
            \_ ->
                let
                    currency =
                        Maybe.withDefault Currency.euro <| Currency.currencyFromCountry jsonResult.country

                    res =
                        List.map (Trigger.jsonToTrigger currency) jsonResult.triggers
                in
                Expect.equal res transformed.triggers
        ]
