module AmountCondition exposing (AmountCondition(..), amountConditionNames, amountConditions, defaultAmount, defaultCondition, defaultPercent, fromName, fromValue, toName, valueStr, valueType)

import BaseTypes exposing (Amount, Currency, Percent, amount, amountFromStr, amountToStr, percent, percentFromStr, percentToStr)


type AmountCondition
    = LessThan Amount
    | GreaterThan Amount
    | Equals Amount
    | IncreasedByPercent Percent
    | DecreasedByPercent Percent
    | IncreasedByAmount Amount
    | DecreasedByAmount Amount


{-| The "Enum" representation.
-}
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


defaultCondition : Currency -> AmountCondition
defaultCondition =
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
            "less_than"

        GreaterThan _ ->
            "greater_than"

        Equals _ ->
            "equals"

        IncreasedByPercent _ ->
            "increases_by"

        DecreasedByPercent _ ->
            "decreases_by"

        IncreasedByAmount _ ->
            "increases_with"

        DecreasedByAmount _ ->
            "decreases_with"


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
        "less_than" ->
            LessThan anAmount

        "greater_than" ->
            GreaterThan anAmount

        "equals" ->
            Equals anAmount

        "increases_by" ->
            IncreasedByPercent defaultPercent

        "decreases_by" ->
            DecreasedByPercent defaultPercent

        "increases_with" ->
            IncreasedByAmount anAmount

        "decreases_with" ->
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


valueStr : AmountCondition -> String
valueStr condition =
    case condition of
        LessThan x ->
            amountToStr x

        GreaterThan x ->
            amountToStr x

        Equals x ->
            amountToStr x

        IncreasedByPercent x ->
            percentToStr x

        DecreasedByPercent x ->
            percentToStr x

        IncreasedByAmount x ->
            amountToStr x

        DecreasedByAmount x ->
            amountToStr x


valueType : AmountCondition -> String
valueType condition =
    case condition of
        IncreasedByPercent _ ->
            "percentage"

        DecreasedByPercent _ ->
            "percentage"

        _ ->
            "amount"
