module NatNumCondition exposing (NatNumCondition(..), default, defaultNatNum, defaultPercent, fromName, names, natNumConditions, toName, fromValue)

import BaseTypes exposing (NatNum, Percent, natNum, natNumFromStr, percent, percentFromStr)


type NatNumCondition
    = LessThan NatNum
    | GreaterThan NatNum
    | Equals NatNum
    | IncreasedBy Percent
    | DecreasedBy Percent


natNumConditions : List NatNumCondition
natNumConditions =
    [ LessThan defaultNatNum
    , GreaterThan defaultNatNum
    , Equals defaultNatNum
    , IncreasedBy defaultPercent
    , DecreasedBy defaultPercent
    ]


default : NatNumCondition
default =
    LessThan defaultNatNum


defaultNatNum : NatNum
defaultNatNum =
    natNum 0


defaultPercent : Percent
defaultPercent =
    percent 0


toName : NatNumCondition -> String
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


names =
    List.map toName natNumConditions


fromName : String -> NatNumCondition
fromName name =
    case name of
        "LessThan" ->
            LessThan defaultNatNum

        "GreaterThan" ->
            GreaterThan defaultNatNum

        "Equals" ->
            Equals defaultNatNum

        "IncreasedByPercent" ->
            IncreasedBy defaultPercent

        "DecreasedByPercent" ->
            DecreasedBy defaultPercent

        _ ->
            LessThan defaultNatNum


fromValue : NatNumCondition -> String -> NatNumCondition
fromValue condition string =
    let
        natNumWithDefault =
            Maybe.withDefault defaultNatNum << natNumFromStr

        percentWithDefault =
            Maybe.withDefault defaultPercent << percentFromStr
    in
    string
        |> (case condition of
                LessThan _ ->
                    LessThan << natNumWithDefault

                GreaterThan _ ->
                    GreaterThan << natNumWithDefault

                Equals _ ->
                    Equals << natNumWithDefault

                IncreasedBy _ ->
                    IncreasedBy << percentWithDefault

                DecreasedBy _ ->
                    DecreasedBy << percentWithDefault
           )
