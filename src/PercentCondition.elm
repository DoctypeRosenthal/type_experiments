module PercentCondition exposing (PercentCondition(..), default, defaultPercent, fromName, percentConditionNames, percentConditions, toName, fromValue)

import BaseTypes exposing (Percent, percent, percentFromStr)


type PercentCondition
    = LessThan Percent
    | GreaterThan Percent
    | Equals Percent
    | IncreasedBy Percent
    | DecreasedBy Percent


percentConditions : List PercentCondition
percentConditions =
    [ LessThan defaultPercent
    , GreaterThan defaultPercent
    , Equals defaultPercent
    , IncreasedBy defaultPercent
    , DecreasedBy defaultPercent
    ]


default : PercentCondition
default =
    LessThan defaultPercent


defaultPercent : Percent
defaultPercent =
    percent 0


toName : PercentCondition -> String
toName condition =
    case condition of
        LessThan _ ->
            "LessThan"

        GreaterThan _ ->
            "GreaterThan"

        Equals _ ->
            "Equals"

        IncreasedBy _ ->
            "IncreasedByPercent"

        DecreasedBy _ ->
            "DecreasedByPercent"


percentConditionNames =
    List.map toName percentConditions


fromName : String -> PercentCondition
fromName name =
    case name of
        "LessThan" ->
            LessThan defaultPercent

        "GreaterThan" ->
            GreaterThan defaultPercent

        "Equals" ->
            Equals defaultPercent

        "IncreasedBy" ->
            IncreasedBy defaultPercent

        "DecreasedBy" ->
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
