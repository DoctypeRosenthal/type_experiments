module Trigger exposing (OptionsView, Trigger(..), TriggerView, ValueView(..), allCriteriaNames, conditionsView, criteriaView, criterionName, defaultTrigger, fromConditionName, fromCriterionName, fromValue, toView)

import AmountCondition exposing (AmountCondition, amountConditionNames)
import BaseTypes exposing (Amount, Currency, NatNum, Percent, amountToStr, natNumToStr, percentToStr)
import NatNumCondition exposing (NatNumCondition, names)
import PercentCondition exposing (PercentCondition, percentConditionNames)
import Status exposing (allStatus)
import StatusCondition exposing (StatusCondition, allNames)



-- MODEL


type Trigger
    = Clicks NatNumCondition
    | Impressions NatNumCondition
    | ACoS PercentCondition
    | CTR PercentCondition
    | Sales AmountCondition
    | Cost AmountCondition
    | CPC AmountCondition
    | Status StatusCondition


allTriggers : List Trigger
allTriggers =
    [ Clicks NatNumCondition.default
    , Impressions NatNumCondition.default
    , ACoS PercentCondition.default
    , CTR PercentCondition.default
    , Sales AmountCondition.default
    , Cost AmountCondition.default
    , CPC AmountCondition.default
    , Status StatusCondition.default
    ]


defaultTrigger : Trigger
defaultTrigger =
    Clicks NatNumCondition.default



-- CONSTRUCTORS FROM STRINGS


fromCriterionName : String -> Trigger
fromCriterionName string =
    case string of
        "Clicks" ->
            Clicks NatNumCondition.default

        "Impressions" ->
            Impressions NatNumCondition.default

        "ACoS" ->
            ACoS PercentCondition.default

        "CTR" ->
            CTR PercentCondition.default

        "Sales" ->
            Sales AmountCondition.default

        "Cost" ->
            Cost AmountCondition.default

        "CPC" ->
            CPC AmountCondition.default

        "Status" ->
            Status StatusCondition.default

        _ ->
            Clicks NatNumCondition.default


fromConditionName : Trigger -> String -> Trigger
fromConditionName trigger name =
    name
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
                    Sales << AmountCondition.fromName

                Cost _ ->
                    Cost << AmountCondition.fromName

                CPC _ ->
                    CPC << AmountCondition.fromName

                Status _ ->
                    Status << StatusCondition.fromName
           )


fromValue : Currency -> Trigger -> String -> Trigger
fromValue currency trigger string =
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

                Status condition ->
                    Status << StatusCondition.fromValue condition
           )



--setCondition : Trigger -> String -> Trigger
--
--
--setValue : Trigger -> String -> Trigger
-- VIEW
-- Translations


criterionName : Trigger -> String
criterionName trigger =
    case trigger of
        Clicks _ ->
            "Clicks"

        Impressions _ ->
            "Impressions"

        ACoS _ ->
            "ACoS"

        CTR _ ->
            "CTR"

        Sales _ ->
            "Sales"

        Cost _ ->
            "Cost"

        CPC _ ->
            "CPC"

        Status _ ->
            "Status"


allCriteriaNames : List String
allCriteriaNames =
    List.map criterionName allTriggers


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
toView : Trigger -> TriggerView
toView trigger =
    TriggerView (criteriaView trigger) (conditionsView trigger) (valueView trigger)


criteriaView : Trigger -> OptionsView
criteriaView trigger =
    OptionsView allCriteriaNames <| criterionName trigger


natNumOptions =
    OptionsView names << NatNumCondition.toName


percentOptions =
    OptionsView percentConditionNames << PercentCondition.toName


amountOptions =
    OptionsView amountConditionNames << AmountCondition.toName


statusOptions =
    OptionsView allNames << StatusCondition.toName


conditionsView : Trigger -> OptionsView
conditionsView trigger =
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
            amountOptions condition

        Cost condition ->
            amountOptions condition

        CPC condition ->
            amountOptions condition

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
        StatusCondition.ChangedTo val ->
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

        Status condition ->
            statusValView condition
