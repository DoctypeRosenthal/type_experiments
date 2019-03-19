module Rule exposing (Model, Monitor(..), Msg(..), allMonitors, decoder, defaultMonitor, encode, init, monitorToString, supportedMonitorTriggers, update, view)

import BaseTypes exposing (natNumToStr, percentToStr)
import Currency exposing (Currency)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Trigger exposing (Trigger)


type alias Model =
    { monitor : Monitor
    , triggers : List Trigger
    , currency : Currency
    , triggerLogic : Trigger.Logic
    }


type alias ModelJson =
    { country : String
    , monitor_type : String
    , condition_matching : Int
    , triggers : List Trigger.TriggerJson
    }


type Msg
    = AddTrigger
    | RemoveTrigger Trigger.Index
    | SetTriggerCriterion Trigger.Index String
    | SetTriggerCondition Trigger.Index Trigger String
    | SetTriggerValue Trigger.Index Trigger String
    | SetMonitor Monitor
    | SetTriggerLogic Bool


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


monitorFromString : String -> Maybe Monitor
monitorFromString monitorString =
    case monitorString of
        "campaigns" ->
            Just MonitorCampaigns

        "keywords" ->
            Just MonitorKeywords

        "asins" ->
            Just MonitorAsins

        _ ->
            Nothing


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
      , currency = Currency.euro
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        replaceTrigger : Trigger.Index -> Trigger -> ( Model, Cmd Msg )
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
            ( { model | triggers = model.triggers ++ [ Trigger.defaultTrigger ] }
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


view : Model -> Html Msg
view model =
    Html.div []
        ([ Html.img [ Html.Attributes.src "/logo.svg" ] []
         , Html.h2 [] [ Html.text "Monitor Types" ]
         , Html.p [] <| List.map (monitorToHtml SetMonitor model.monitor) allMonitors
         , Html.h2 [] [ Html.text "Triggers" ]
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
            numberInput [ Html.Attributes.step "0.1" ] handleChange (Currency.symbolFromAmount amount) (Currency.amountToStr amount)

        Trigger.PercentInput percent ->
            numberInput [ Html.Attributes.step "1" ] handleChange "%" (percentToStr percent)

        Trigger.NatNumInput natNum ->
            numberInput [ Html.Attributes.step "1" ] handleChange "" (natNumToStr natNum)

        Trigger.StatusSelect optionsView ->
            triggerOptionsToHtml handleChange optionsView


{-| This function should be in this module as it defines HOW to exactly render the Trigger in this context.
-}
triggerToHtml : Currency -> List Trigger -> Trigger.Index -> Trigger -> Html Msg
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



-- DE/ENCODING


type alias ProjectCountry =
    String


encode : ModelJson -> Json.Encode.Value
encode rule =
    Json.Encode.object
        [ ( "country", Json.Encode.string rule.country )
        , ( "monitor_type", Json.Encode.string rule.monitor_type )
        , ( "condition_matching", Json.Encode.int rule.condition_matching )
        , ( "triggers", Json.Encode.list Trigger.encode rule.triggers )
        ]


decoder : Json.Decode.Decoder ModelJson
decoder =
    let
        withCountry : ProjectCountry -> Json.Decode.Decoder ModelJson
        withCountry country =
            let
                currency =
                    Maybe.withDefault Currency.euro <| Currency.currencyFromCountry country
            in
            Json.Decode.succeed ModelJson
                |> Pipeline.required "country" Json.Decode.string
                |> Pipeline.required "monitor_type" Json.Decode.string
                |> Pipeline.required "condition_matching" Json.Decode.int
                |> Pipeline.required "triggers" (Json.Decode.list (Trigger.decode currency))
    in
    Json.Decode.succeed withCountry
        |> Pipeline.required "country" Json.Decode.string
        |> Pipeline.resolve


decodeMonitor : String -> Json.Decode.Decoder Monitor
decodeMonitor monitorString =
    case monitorFromString monitorString of
        Just monitor ->
            Json.Decode.succeed monitor

        Nothing ->
            Json.Decode.fail (monitorString ++ " is not supported")


encodeMonitor : Monitor -> Json.Encode.Value
encodeMonitor =
    monitorToString >> Json.Encode.string
