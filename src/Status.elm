module Status exposing (Status(..), allNames, allStatus, fromString, toName)

{-| Implements the bid automation-specific Status type.
-}


type Status
    = Enabled
    | Paused
    | Archived


allStatus : List Status
allStatus =
    [ Enabled
    , Paused
    , Archived
    ]


toName : Status -> String
toName status =
    case status of
        Paused ->
            "Paused"

        Enabled ->
            "Enabled"

        Archived ->
            "Archived"


fromString : String -> Status
fromString string =
    case string of
        "Paused" ->
            Paused

        "Enabled" ->
            Enabled

        "Archived" ->
            Archived

        _ ->
            Paused


allNames : List String
allNames =
    List.map toName allStatus
