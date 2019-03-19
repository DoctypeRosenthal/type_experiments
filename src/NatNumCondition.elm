module NatNumCondition exposing (NatNumCondition(..), defaultCondition, defaultNatNum, defaultPercent, fromName, fromValue, names, natNumConditions, toName, valueStr, valueType)

import BaseTypes.NatNum as NatNum exposing (NatNum)
import BaseTypes.Percent as Percent exposing (Percent)


type NatNumCondition
    = LessThan NatNum
    | GreaterThan NatNum
    | Equals NatNum
    | IncreasedBy Percent
    | DecreasedBy Percent


{-| The "Enum" representation.
-}
natNumConditions : List NatNumCondition
natNumConditions =
    [ LessThan defaultNatNum
    , GreaterThan defaultNatNum
    , Equals defaultNatNum
    , IncreasedBy defaultPercent
    , DecreasedBy defaultPercent
    ]


defaultCondition : NatNumCondition
defaultCondition =
    LessThan defaultNatNum


defaultNatNum : NatNum
defaultNatNum =
    NatNum.fromInt 0


defaultPercent : Percent
defaultPercent =
    Percent.fromInt 0


toName : NatNumCondition -> String
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


names =
    List.map toName natNumConditions


fromName : String -> NatNumCondition
fromName name =
    case name of
        "less_than" ->
            LessThan defaultNatNum

        "greater_than" ->
            GreaterThan defaultNatNum

        "equals" ->
            Equals defaultNatNum

        "increases_by" ->
            IncreasedBy defaultPercent

        "decreases_by" ->
            DecreasedBy defaultPercent

        _ ->
            LessThan defaultNatNum


fromValue : NatNumCondition -> String -> NatNumCondition
fromValue condition string =
    let
        natNumWithDefault =
            Maybe.withDefault defaultNatNum << NatNum.fromStr

        percentWithDefault =
            Maybe.withDefault defaultPercent << Percent.fromStr
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


valueStr : NatNumCondition -> String
valueStr condition =
    case condition of
        LessThan x ->
            NatNum.toStr x

        GreaterThan x ->
            NatNum.toStr x

        Equals x ->
            NatNum.toStr x

        IncreasedBy x ->
            Percent.toStr x

        DecreasedBy x ->
            Percent.toStr x


valueType : NatNumCondition -> String
valueType condition =
    case condition of
        IncreasedBy x ->
            "percentage"

        DecreasedBy x ->
            "percentage"

        _ ->
            "natural_number"
