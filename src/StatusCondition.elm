module StatusCondition exposing (StatusCondition(..), allNames, default, defaultStatus, fromName, fromValue, statusConditions, toName)

import Status exposing (Status(..))


type StatusCondition
    = ChangedTo Status


statusConditions : List StatusCondition
statusConditions =
    [ ChangedTo defaultStatus
    ]


default : StatusCondition
default =
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
