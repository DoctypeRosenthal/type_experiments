module AmountCondition exposing (AmountCondition(..), amountConditionNames, amountConditions, default, defaultAmount, defaultPercent, fromName, fromValue, toName)

import BaseTypes exposing (Amount, Currency, Percent, amount, amountFromStr, percent, percentFromStr)


type AmountCondition
    = LessThan Amount
    | GreaterThan Amount
    | Equals Amount
    | IncreasedByPercent Percent
    | DecreasedByPercent Percent
    | IncreasedByAmount Amount
    | DecreasedByAmount Amount


amountConditions : Currency -> List AmountCondition
amountConditions currency =
    let
        anAmount =
            defaultAmount currency
    in
    [ LessThan anAmount
    , GreaterThan anAmount
    , Equals anAmount
    , IncreasedByPercent defaultPercent
    , DecreasedByPercent defaultPercent
    , IncreasedByAmount anAmount
    , DecreasedByAmount anAmount
    ]


default : Currency -> AmountCondition
default =
    LessThan << defaultAmount


defaultAmount : Currency -> Amount
defaultAmount currency =
    amount currency 0


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


amountConditionNames : Currency -> List String
amountConditionNames currency =
    List.map toName (amountConditions currency)


fromName : Currency -> String -> AmountCondition
fromName currency name =
    let
        anAmount =
            defaultAmount currency
    in
    case name of
        "LessThan" ->
            LessThan anAmount

        "GreaterThan" ->
            GreaterThan anAmount

        "Equals" ->
            Equals anAmount

        "IncreasedByPercent" ->
            IncreasedByPercent defaultPercent

        "DecreasedByPercent" ->
            DecreasedByPercent defaultPercent

        "IncreasedByAmount" ->
            IncreasedByAmount anAmount

        "DecreasedByAmount" ->
            DecreasedByAmount anAmount

        _ ->
            LessThan anAmount


fromValue : Currency -> AmountCondition -> String -> AmountCondition
fromValue currency condition string =
    let
        amountWithDefault =
            Maybe.withDefault (defaultAmount currency) << amountFromStr currency

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
