module AmountCondition exposing (AmountCondition(..), amountConditionNames, amountConditions, default, defaultAmount, defaultPercent, fromName, fromValue, toName)

import BaseTypes exposing (Amount, Currency, Percent, amount, amountFromStr, euro, percent, percentFromStr)


type AmountCondition
    = LessThan Amount
    | GreaterThan Amount
    | Equals Amount
    | IncreasedByPercent Percent
    | DecreasedByPercent Percent
    | IncreasedByAmount Amount
    | DecreasedByAmount Amount


amountConditions : List AmountCondition
amountConditions =
    [ LessThan defaultAmount
    , GreaterThan defaultAmount
    , Equals defaultAmount
    , IncreasedByPercent defaultPercent
    , DecreasedByPercent defaultPercent
    , IncreasedByAmount defaultAmount
    , DecreasedByAmount defaultAmount
    ]


default : AmountCondition
default =
    LessThan defaultAmount


defaultAmount : Amount
defaultAmount =
    amount euro 0


defaultPercent : Percent
defaultPercent =
    percent 0


toName : AmountCondition -> String
toName condition =
    case condition of
        LessThan _ ->
            "LessThan"

        GreaterThan _ ->
            "GreaterThan"

        Equals _ ->
            "Equals"

        IncreasedByPercent _ ->
            "IncreasedByPercent"

        DecreasedByPercent _ ->
            "DecreasedByPercent"

        IncreasedByAmount _ ->
            "IncreasedByAmount"

        DecreasedByAmount _ ->
            "DecreasedByAmount"


amountConditionNames =
    List.map toName amountConditions


fromName : String -> AmountCondition
fromName name =
    case name of
        "LessThan" ->
            LessThan defaultAmount

        "GreaterThan" ->
            GreaterThan defaultAmount

        "Equals" ->
            Equals defaultAmount

        "IncreasedByPercent" ->
            IncreasedByPercent defaultPercent

        "DecreasedByPercent" ->
            DecreasedByPercent defaultPercent

        "IncreasedByAmount" ->
            IncreasedByAmount defaultAmount

        "DecreasedByAmount" ->
            DecreasedByAmount defaultAmount

        _ ->
            LessThan defaultAmount


fromValue : Currency -> AmountCondition -> String -> AmountCondition
fromValue currency condition string =
    let
        amountWithDefault =
            Maybe.withDefault defaultAmount << amountFromStr currency

        percentWithDefault =
            Maybe.withDefault defaultPercent << percentFromStr
    in
    string
        |> (case condition of
                LessThan _ ->
                    LessThan << amountWithDefault

                GreaterThan _ ->
                    GreaterThan << amountWithDefault

                Equals _ ->
                    Equals << amountWithDefault

                IncreasedByPercent _ ->
                    IncreasedByPercent << percentWithDefault

                DecreasedByPercent _ ->
                    DecreasedByPercent << percentWithDefault

                IncreasedByAmount _ ->
                    IncreasedByAmount << amountWithDefault

                DecreasedByAmount _ ->
                    DecreasedByAmount << amountWithDefault
           )
