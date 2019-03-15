module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import BaseTypes exposing (Currency, amountToStr, natNumToStr, percentToStr, symbolFromAmount, usd)
import Browser
import Html exposing (Html, div, h1, h2, img, p, text)
import Html.Attributes exposing (src)
import Html.Events
import Trigger exposing (Trigger, TriggerView, defaultTrigger)



---- MODEL ----


type alias Model =
    { triggers : List Trigger
    , currency : Currency
    }


init : ( Model, Cmd Msg )
init =
    ( { triggers = [], currency = usd }, Cmd.none )



---- UPDATE ----


type alias Index =
    Int


type Msg
    = AddTrigger
    | RemoveTrigger Index
    | SetTriggerCriterion Index String
    | SetTriggerCondition Index Trigger String
    | SetTriggerValue Index Trigger String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTrigger ->
            ( { model | triggers = defaultTrigger :: model.triggers }
            , Cmd.none
            )

        RemoveTrigger index ->
            ( { model | triggers = model.triggers |> List.indexedMap Tuple.pair |> List.filter (\( i, _ ) -> i /= index) |> List.map Tuple.second }
            , Cmd.none
            )

        SetTriggerCriterion index string ->
            ( { model
                | triggers =
                    List.indexedMap
                        (\i x ->
                            if i /= index then
                                x

                            else
                                Trigger.fromCriterionName string
                        )
                        model.triggers
              }
            , Cmd.none
            )

        SetTriggerCondition index trigger string ->
            ( { model
                | triggers =
                    List.indexedMap
                        (\i x ->
                            if i /= index then
                                x

                            else
                                Trigger.fromConditionName trigger string
                        )
                        model.triggers
              }
            , Cmd.none
            )

        SetTriggerValue index trigger string ->
            ( { model
                | triggers =
                    List.indexedMap
                        (\i x ->
                            if i /= index then
                                x

                            else
                                Trigger.fromValue model.currency trigger string
                        )
                        model.triggers
              }
            , Cmd.none
            )



---- VIEW ----


triggerOptionsToHtml : (String -> Msg) -> Trigger.OptionsView -> Html Msg
triggerOptionsToHtml handleSelection { available, selected } =
    Html.select [ Html.Events.onInput handleSelection ]
        (List.map
            (\x -> Html.option [ Html.Attributes.value x, Html.Attributes.selected (x == selected) ] [ Html.text x ])
            available
        )


numberInput : List (Html.Attribute Msg) -> (String -> Msg) -> String -> String -> Html Msg
numberInput attrs handleChange symbol value =
    Html.span []
        [ Html.input
            (attrs ++ [ Html.Attributes.type_ "number", Html.Events.onInput handleChange, Html.Attributes.value value ])
            []
        , Html.text symbol
        ]


triggerValueToHtml : (String -> Msg) -> Trigger.ValueView -> Html Msg
triggerValueToHtml handleChange valueView =
    case valueView of
        Trigger.AmountInput amount ->
            numberInput [ Html.Attributes.step "0.1" ] handleChange (symbolFromAmount amount) (amountToStr amount)

        Trigger.PercentInput percent ->
            numberInput [ Html.Attributes.step "1" ] handleChange "%" (percentToStr percent)

        Trigger.NatNumInput natNum ->
            numberInput [ Html.Attributes.step "1" ] handleChange "" (natNumToStr natNum)

        Trigger.StatusSelect optionsView ->
            triggerOptionsToHtml handleChange optionsView


{-| This function should be in this module as it defines HOW to exactly render the Trigger in this context.
-}
triggerToHtml : Index -> Trigger -> Html Msg
triggerToHtml index trigger =
    let
        { criteria, conditions, value } =
            Trigger.toView trigger
    in
    Html.p []
        [ triggerOptionsToHtml (SetTriggerCriterion index) criteria
        , triggerOptionsToHtml (SetTriggerCondition index trigger) conditions
        , triggerValueToHtml (SetTriggerValue index trigger) value
        , Html.button [ Html.Events.onClick <| RemoveTrigger index ] [ Html.text "x" ]
        ]


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
         , h2 [] [ text "Triggers" ]
         , Html.button [ Html.Events.onClick AddTrigger ] [ Html.text "add trigger" ]
         ]
            ++ List.indexedMap triggerToHtml model.triggers
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }