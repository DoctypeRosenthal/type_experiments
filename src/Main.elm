module Main exposing (Model, Msg(..), init, main, update, view)

import BaseTypes exposing (Currency, amountToStr, euro, natNumToStr, percentToStr, symbolFromAmount, usd)
import Browser
import Html exposing (Html, div, h2, img, text)
import Html.Attributes exposing (src)
import Html.Events
import Trigger exposing (Trigger, TriggerView, defaultTrigger)



---- MODEL ----


type alias Model =
    { monitor : Monitor
    , triggers : List Trigger
    , currency : Currency
    , triggerLogic : Trigger.Logic
    }


type Monitor
    = MonitorCampaigns
    | MonitorKeywords
    | MonitorAsins


allMonitors : List Monitor
allMonitors =
    [ MonitorCampaigns
    , MonitorKeywords
    , MonitorAsins
    ]


defaultMonitor : Monitor
defaultMonitor =
    MonitorCampaigns


monitorToString : Monitor -> String
monitorToString monitor =
    case monitor of
        MonitorCampaigns ->
            "campaigns"

        MonitorKeywords ->
            "keywords"

        MonitorAsins ->
            "asins"


supportedMonitorTriggers : Monitor -> Currency -> List Trigger
supportedMonitorTriggers monitor currency =
    let
        allTriggers =
            Trigger.allTriggers currency

        withoutBid =
            List.filter (not << Trigger.isBid) allTriggers
    in
    case monitor of
        MonitorCampaigns ->
            withoutBid

        MonitorKeywords ->
            allTriggers

        MonitorAsins ->
            withoutBid


init : ( Model, Cmd Msg )
init =
    ( { monitor = defaultMonitor
      , triggerLogic = Trigger.And
      , triggers = []
      , currency = euro
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Index =
    Int


type Msg
    = AddTrigger
    | RemoveTrigger Index
    | SetTriggerCriterion Index String
    | SetTriggerCondition Index Trigger String
    | SetTriggerValue Index Trigger String
    | SetMonitor Monitor
    | SetTriggerLogic Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        replaceTrigger : Index -> Trigger -> ( Model, Cmd Msg )
        replaceTrigger index nextTrigger =
            ( { model
                | triggers =
                    List.indexedMap
                        (\i x ->
                            if i /= index then
                                x

                            else
                                nextTrigger
                        )
                        model.triggers
              }
            , Cmd.none
            )
    in
    case msg of
        AddTrigger ->
            ( { model | triggers = model.triggers ++ [ defaultTrigger ] }
            , Cmd.none
            )

        RemoveTrigger index ->
            ( { model | triggers = model.triggers |> List.indexedMap Tuple.pair |> List.filter (\( i, _ ) -> i /= index) |> List.map Tuple.second }
            , Cmd.none
            )

        SetTriggerCriterion index string ->
            replaceTrigger index (Trigger.fromCriterionKey model.currency string)

        SetTriggerCondition index trigger string ->
            replaceTrigger index (Trigger.fromConditionKey model.currency string trigger)

        SetTriggerValue index trigger string ->
            replaceTrigger index (Trigger.fromValue model.currency string trigger)

        SetMonitor monitor ->
            -- important : reset triggers to avoid invalid monitor-trigger-combinations!
            ( { model | monitor = monitor, triggers = [] }, Cmd.none )

        SetTriggerLogic setToAnd ->
            ( { model
                | triggerLogic =
                    if setToAnd then
                        Trigger.And

                    else
                        Trigger.Or
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
triggerToHtml : Currency -> List Trigger -> Index -> Trigger -> Html Msg
triggerToHtml currency supportedTriggers index trigger =
    let
        { criteria, conditions, value } =
            Trigger.toView currency trigger supportedTriggers
    in
    Html.p []
        [ triggerOptionsToHtml (SetTriggerCriterion index) criteria
        , triggerOptionsToHtml (SetTriggerCondition index trigger) conditions
        , triggerValueToHtml (SetTriggerValue index trigger) value
        , Html.button [ Html.Events.onClick <| RemoveTrigger index ] [ Html.text "x" ]
        ]


monitorToHtml : (Monitor -> Msg) -> Monitor -> Monitor -> Html Msg
monitorToHtml handleClick active x =
    Html.span []
        [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.value <| monitorToString x, Html.Attributes.checked <| x == active, Html.Events.onClick <| handleClick x ] []
        , Html.span [] [ Html.text <| monitorToString x ]
        ]


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
         , h2 [] [ text "Monitor Types" ]
         , Html.p [] <| List.map (monitorToHtml SetMonitor model.monitor) allMonitors
         , h2 [] [ text "Triggers" ]
         , Html.p []
            [ Html.text "Alle Trigger müssen zutreffen"
            , Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.selected <| model.triggerLogic == Trigger.And
                , Html.Events.onCheck SetTriggerLogic
                ]
                []
            ]
         ]
            ++ List.indexedMap (triggerToHtml model.currency <| supportedMonitorTriggers model.monitor model.currency) model.triggers
            ++ [ Html.button [ Html.Events.onClick AddTrigger ] [ Html.text "add trigger" ] ]
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
