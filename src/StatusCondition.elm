module StatusCondition exposing (StatusCondition(..), allNames, defaultCondition, defaultStatus, fromName, fromValue, statusConditions, toName, valueStr)

import Status exposing (Status(..))


type StatusCondition
    = ChangedTo Status


{-| The "Enum" representation.
-}
statusConditions : List StatusCondition
statusConditions =
    [ ChangedTo defaultStatus
    ]


defaultCondition : StatusCondition
defaultCondition =
    ChangedTo defaultStatus


defaultStatus : Status
defaultStatus =
    Paused


toName : StatusCondition -> String
toName condition =
    case condition of
        ChangedTo _ ->
            "ChangedTo"


allNames =
    List.map toName statusConditions


fromName : String -> StatusCondition
fromName _ =
    -- for now only one possible status condition. Unlikely to change in the future.
    ChangedTo defaultStatus


fromValue : StatusCondition -> String -> StatusCondition
fromValue condition =
    -- for now only one possible condition.
    ChangedTo << Status.fromString


valueStr : StatusCondition -> String
valueStr (ChangedTo x) =
    Status.toName x
