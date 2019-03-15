module PercentCondition exposing (PercentCondition(..), defaultCondition, defaultPercent, fromName, fromValue, percentConditionNames, percentConditions, toName, valueStr, valueType)

import BaseTypes exposing (Percent, percent, percentFromStr, percentToStr)


type PercentCondition
    = LessThan Percent
    | GreaterThan Percent
    | Equals Percent
    | IncreasedBy Percent
    | DecreasedBy Percent


{-| The "Enum" representation.
-}
percentConditions : List PercentCondition
percentConditions =
    [ LessThan defaultPercent
    , GreaterThan defaultPercent
    , Equals defaultPercent
    , IncreasedBy defaultPercent
    , DecreasedBy defaultPercent
    ]


defaultCondition : PercentCondition
defaultCondition =
    LessThan defaultPercent


defaultPercent : Percent
defaultPercent =
    percent 0


toName : PercentCondition -> String
toName condition =
    case condition of
        LessThan _ ->
            "less_than"

        GreaterThan _ ->
            "greater_than"

        Equals _ ->
            "equals"

        IncreasedBy _ ->
            "increases_by"

        DecreasedBy _ ->
            "decreases_by"


percentConditionNames =
    List.map toName percentConditions


fromName : String -> PercentCondition
fromName name =
    case name of
        "less_than" ->
            LessThan defaultPercent

        "greater_than" ->
            GreaterThan defaultPercent

        "equals" ->
            Equals defaultPercent

        "increases_by" ->
            IncreasedBy defaultPercent

        "decreases_by" ->
            DecreasedBy defaultPercent

        _ ->
            LessThan defaultPercent


fromValue : PercentCondition -> String -> PercentCondition
fromValue condition string =
    let
        percentWithDefault =
            Maybe.withDefault defaultPercent << percentFromStr
    in
    string
        |> (case condition of
                LessThan _ ->
                    LessThan << percentWithDefault

                GreaterThan _ ->
                    GreaterThan << percentWithDefault

                Equals _ ->
                    Equals << percentWithDefault

                IncreasedBy _ ->
                    IncreasedBy << percentWithDefault

                DecreasedBy _ ->
                    DecreasedBy << percentWithDefault
           )


valueStr : PercentCondition -> String
valueStr condition =
    percentToStr <|
        case condition of
            LessThan x ->
                x

            GreaterThan x ->
                x

            Equals x ->
                x

            IncreasedBy x ->
                x

            DecreasedBy x ->
                x


valueType : PercentCondition -> String
valueType _ =
    -- Only one possiblitiy for now. Param is only there to make the API equal to the other conditions.
    "percentage"
