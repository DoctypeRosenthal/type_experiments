module StatusCondition exposing (StatusCondition(..), allNames, defaultCondition, defaultStatus, fromName, fromValue, statusConditions, toName, valueStr, valueType)

import Status exposing (Status(..))


type StatusCondition
    = Equals Status


{-| The "Enum" representation.
-}
statusConditions : List StatusCondition
statusConditions =
    [ Equals defaultStatus
    ]


defaultCondition : StatusCondition
defaultCondition =
    Equals defaultStatus


defaultStatus : Status
defaultStatus =
    Paused


toName : StatusCondition -> String
toName condition =
    case condition of
        Equals _ ->
            "status_change"


allNames =
    List.map toName statusConditions


fromName : String -> StatusCondition
fromName _ =
    -- for now only one possible status condition. Unlikely to change in the future.
    Equals defaultStatus


fromValue : StatusCondition -> String -> StatusCondition
fromValue condition =
    -- for now only one possible condition.
    Equals << Status.fromString


valueStr : StatusCondition -> String
valueStr (Equals x) =
    Status.toName x


valueType : StatusCondition -> String
valueType _ =
    -- Param is only there to equalize interface among all conditions.
    "status"
