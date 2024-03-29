module Trigger exposing (Index, Logic(..), OptionsView, Trigger(..), TriggerJson, TriggerView, ValueView(..), allCriterionKeys, allTriggers, conditionsView, criteriaView, criterionKey, decode, decodeLogic, defaultTrigger, encode, encodeLogic, fromConditionKey, fromCriterionKey, fromValue, isBid, jsonToTrigger, toView, triggerToJson)

import AmountCondition exposing (AmountCondition, amountConditionNames)
import BaseTypes.NatNum exposing (NatNum)
import BaseTypes.Percent exposing (Percent)
import Currency exposing (Amount, Currency)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import NatNumCondition exposing (NatNumCondition, names)
import PercentCondition exposing (PercentCondition, percentConditionNames)
import Status
import StatusCondition exposing (StatusCondition, allNames)



-- MODEL


type alias Index =
    Int


type Logic
    = And
    | Or


{-| This nested structure guarantees at compile time that there will be no invalid combinations of criteria, conditions
and values. Test-free!
So if you start implement a new Trigger type, start here!
-}
type Trigger
    = Clicks NatNumCondition
    | Impressions NatNumCondition
    | ACoS PercentCondition
    | CTR PercentCondition
    | Sales AmountCondition
    | Cost AmountCondition
    | CPC AmountCondition
    | Bid AmountCondition
    | Status StatusCondition


allTriggers : Currency -> List Trigger
allTriggers currency =
    let
        defaultAmountCondition =
            AmountCondition.defaultCondition currency
    in
    [ Clicks NatNumCondition.defaultCondition
    , Impressions NatNumCondition.defaultCondition
    , ACoS PercentCondition.defaultCondition
    , CTR PercentCondition.defaultCondition
    , Sales defaultAmountCondition
    , Cost defaultAmountCondition
    , CPC defaultAmountCondition
    , Bid defaultAmountCondition
    , Status StatusCondition.defaultCondition
    ]


isBid : Trigger -> Bool
isBid x =
    case x of
        Bid _ ->
            True

        _ ->
            False


defaultTrigger : Trigger
defaultTrigger =
    Clicks NatNumCondition.defaultCondition



-- CONSTRUCTORS FROM STRINGS


type alias CriterionKey =
    String


fromCriterionKey : Currency -> CriterionKey -> Trigger
fromCriterionKey currency key =
    let
        defaultAmountCondition =
            AmountCondition.defaultCondition currency
    in
    case key of
        "clicks" ->
            Clicks NatNumCondition.defaultCondition

        "impressions" ->
            Impressions NatNumCondition.defaultCondition

        "acos" ->
            ACoS PercentCondition.defaultCondition

        "ctr" ->
            CTR PercentCondition.defaultCondition

        "sales" ->
            Sales defaultAmountCondition

        "cost" ->
            Cost defaultAmountCondition

        "cpc" ->
            CPC defaultAmountCondition

        "bid" ->
            Bid defaultAmountCondition

        "status" ->
            Status StatusCondition.defaultCondition

        _ ->
            Clicks NatNumCondition.defaultCondition


criterionKey : Trigger -> CriterionKey
criterionKey trigger =
    case trigger of
        Clicks _ ->
            "clicks"

        Impressions _ ->
            "impressions"

        ACoS _ ->
            "acos"

        CTR _ ->
            "ctr"

        Sales _ ->
            "sales"

        Cost _ ->
            "cost"

        CPC _ ->
            "cpc"

        Bid _ ->
            "bid"

        Status _ ->
            "status"


fromConditionKey : Currency -> String -> Trigger -> Trigger
fromConditionKey currency key trigger =
    let
        amountCondition =
            AmountCondition.fromName currency
    in
    key
        |> (case trigger of
                Clicks _ ->
                    Clicks << NatNumCondition.fromName

                Impressions _ ->
                    Impressions << NatNumCondition.fromName

                ACoS _ ->
                    ACoS << PercentCondition.fromName

                CTR _ ->
                    CTR << PercentCondition.fromName

                Sales _ ->
                    Sales << amountCondition

                Cost _ ->
                    Cost << amountCondition

                CPC _ ->
                    CPC << amountCondition

                Bid _ ->
                    Bid << amountCondition

                Status _ ->
                    Status << StatusCondition.fromName
           )


type alias ConditionKey =
    String


conditionKey : Trigger -> ConditionKey
conditionKey trigger =
    case trigger of
        Clicks condition ->
            NatNumCondition.toName condition

        Impressions condition ->
            NatNumCondition.toName condition

        ACoS condition ->
            PercentCondition.toName condition

        CTR condition ->
            PercentCondition.toName condition

        Sales condition ->
            AmountCondition.toName condition

        Cost condition ->
            AmountCondition.toName condition

        CPC condition ->
            AmountCondition.toName condition

        Bid condition ->
            AmountCondition.toName condition

        Status condition ->
            StatusCondition.toName condition


fromValue : Currency -> String -> Trigger -> Trigger
fromValue currency string trigger =
    string
        |> (case trigger of
                Clicks condition ->
                    Clicks << NatNumCondition.fromValue condition

                Impressions condition ->
                    Impressions << NatNumCondition.fromValue condition

                ACoS condition ->
                    ACoS << PercentCondition.fromValue condition

                CTR condition ->
                    CTR << PercentCondition.fromValue condition

                Sales condition ->
                    Sales << AmountCondition.fromValue currency condition

                Cost condition ->
                    Cost << AmountCondition.fromValue currency condition

                CPC condition ->
                    CPC << AmountCondition.fromValue currency condition

                Bid condition ->
                    Bid << AmountCondition.fromValue currency condition

                Status condition ->
                    Status << StatusCondition.fromValue condition
           )



--setCondition : Trigger -> String -> Trigger
--
--
--setValue : Trigger -> String -> Trigger
-- VIEW
-- Translations


type ValueView
    = AmountInput Amount
    | PercentInput Percent
    | NatNumInput NatNum
    | StatusSelect OptionsView


type alias OptionsView =
    { available : List String
    , selected : String
    }


{-| View representation of a trigger
-}
type alias TriggerView =
    { criteria : OptionsView
    , conditions : OptionsView
    , value : ValueView
    }


{-| This function should be in this module because it just creates an abstract representation of a trigger view.
-}
toView : Currency -> Trigger -> List Trigger -> TriggerView
toView currency selectedTrigger possibleTriggers =
    TriggerView (criteriaView selectedTrigger possibleTriggers) (conditionsView currency selectedTrigger) (valueView selectedTrigger)


{-| As third argument you can specify a rule to filter all triggers. Keep all Triggers which satisfy the rule.
-}
criteriaView : Trigger -> List Trigger -> OptionsView
criteriaView trigger possibleTriggers =
    let
        allKeys =
            List.map criterionKey possibleTriggers

        activeCriterionKey =
            criterionKey trigger
    in
    OptionsView allKeys activeCriterionKey


natNumOptions =
    OptionsView names << NatNumCondition.toName


percentOptions =
    OptionsView percentConditionNames << PercentCondition.toName


amountOptions currency =
    OptionsView (amountConditionNames currency) << AmountCondition.toName


statusOptions =
    OptionsView allNames << StatusCondition.toName


conditionsView : Currency -> Trigger -> OptionsView
conditionsView currency trigger =
    case trigger of
        Clicks condition ->
            natNumOptions condition

        Impressions condition ->
            natNumOptions condition

        ACoS condition ->
            percentOptions condition

        CTR condition ->
            percentOptions condition

        Sales condition ->
            amountOptions currency condition

        Cost condition ->
            amountOptions currency condition

        CPC condition ->
            amountOptions currency condition

        Bid condition ->
            amountOptions currency condition

        Status condition ->
            statusOptions condition


statusSelect selected =
    StatusSelect <| OptionsView Status.allNames (Status.toName selected)


natNumValView : NatNumCondition -> ValueView
natNumValView condition =
    case condition of
        NatNumCondition.LessThan val ->
            NatNumInput val

        NatNumCondition.GreaterThan val ->
            NatNumInput val

        NatNumCondition.Equals val ->
            NatNumInput val

        NatNumCondition.IncreasedBy val ->
            PercentInput val

        NatNumCondition.DecreasedBy val ->
            PercentInput val


percentValView : PercentCondition -> ValueView
percentValView condition =
    case condition of
        PercentCondition.LessThan val ->
            PercentInput val

        PercentCondition.GreaterThan val ->
            PercentInput val

        PercentCondition.Equals val ->
            PercentInput val

        PercentCondition.IncreasedBy val ->
            PercentInput val

        PercentCondition.DecreasedBy val ->
            PercentInput val


amountValView : AmountCondition -> ValueView
amountValView condition =
    case condition of
        AmountCondition.LessThan val ->
            AmountInput val

        AmountCondition.GreaterThan val ->
            AmountInput val

        AmountCondition.Equals val ->
            AmountInput val

        AmountCondition.IncreasedByPercent val ->
            PercentInput val

        AmountCondition.DecreasedByPercent val ->
            PercentInput val

        AmountCondition.IncreasedByAmount val ->
            AmountInput val

        AmountCondition.DecreasedByAmount val ->
            AmountInput val


statusValView : StatusCondition -> ValueView
statusValView condition =
    case condition of
        StatusCondition.Equals val ->
            statusSelect val


valueView : Trigger -> ValueView
valueView trigger =
    case trigger of
        Clicks condition ->
            natNumValView condition

        Impressions condition ->
            natNumValView condition

        ACoS condition ->
            percentValView condition

        CTR condition ->
            percentValView condition

        Sales condition ->
            amountValView condition

        Cost condition ->
            amountValView condition

        CPC condition ->
            amountValView condition

        Bid condition ->
            amountValView condition

        Status condition ->
            statusValView condition



-- DE/ENCODERS


type alias TriggerJson =
    { field : String
    , condition_key : String
    , value : String
    , value_type : String
    }


allCriterionKeys : Currency -> List String
allCriterionKeys =
    List.map criterionKey << allTriggers


valueToStr : Trigger -> String
valueToStr trigger =
    case trigger of
        Clicks condition ->
            NatNumCondition.valueStr condition

        Impressions condition ->
            NatNumCondition.valueStr condition

        ACoS condition ->
            PercentCondition.valueStr condition

        CTR condition ->
            PercentCondition.valueStr condition

        Sales condition ->
            AmountCondition.valueStr condition

        Cost condition ->
            AmountCondition.valueStr condition

        CPC condition ->
            AmountCondition.valueStr condition

        Bid condition ->
            AmountCondition.valueStr condition

        Status condition ->
            StatusCondition.valueStr condition


valueToType : Trigger -> String
valueToType trigger =
    case trigger of
        Clicks condition ->
            NatNumCondition.valueType condition

        Impressions condition ->
            NatNumCondition.valueType condition

        ACoS condition ->
            PercentCondition.valueType condition

        CTR condition ->
            PercentCondition.valueType condition

        Sales condition ->
            AmountCondition.valueType condition

        Cost condition ->
            AmountCondition.valueType condition

        CPC condition ->
            AmountCondition.valueType condition

        Bid condition ->
            AmountCondition.valueType condition

        Status condition ->
            StatusCondition.valueType condition


{-| This only makes a 1:1 Elm-representation of the JSON structure. No validation and no transformation.
-}
decode : Currency -> Json.Decode.Decoder TriggerJson
decode currency =
    Json.Decode.succeed TriggerJson
        |> required "field" Json.Decode.string
        |> required "condition_key" Json.Decode.string
        |> required "value" Json.Decode.string
        |> required "value_type" Json.Decode.string


encode : TriggerJson -> Json.Encode.Value
encode { value, value_type, condition_key, field } =
    Json.Encode.object
        [ ( "value", Json.Encode.string value )
        , ( "value_type", Json.Encode.string value_type )
        , ( "condition_key", Json.Encode.string condition_key )
        , ( "field", Json.Encode.string field )
        ]



-- TRANSFORMATORS


jsonToTrigger : Currency -> TriggerJson -> Trigger
jsonToTrigger currency json =
    fromCriterionKey currency json.field
        -- JSON has no way of expressing dependencies among fields so everything is possible.
        -- However, in the frontend there are only certain combinations possible (see type Trigger!).
        -- So this step can potentially crash.
        |> fromConditionKey currency json.condition_key
        |> fromValue currency json.value


triggerToJson : Trigger -> TriggerJson
triggerToJson trigger =
    { field = criterionKey trigger
    , condition_key = conditionKey trigger
    , value = valueToStr trigger
    , value_type = valueToType trigger
    }


decodeLogic : Int -> Json.Decode.Decoder Logic
decodeLogic logicInt =
    case logicInt of
        0 ->
            Json.Decode.succeed Or

        1 ->
            Json.Decode.succeed And

        _ ->
            Json.Decode.fail "Unsupported logic flag"


encodeLogic : Logic -> Json.Encode.Value
encodeLogic logic =
    case logic of
        Or ->
            Json.Encode.int 0

        And ->
            Json.Encode.int 1



-- TODOs:
-- * write monitor type dependent filtering of triggers
-- * write tests for decoders and transformators
-- * write triggerToDisplayName and conditionToDisplayName (copy&paste from gitlab)
