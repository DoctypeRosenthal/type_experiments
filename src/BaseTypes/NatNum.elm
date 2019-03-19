module BaseTypes.NatNum exposing (NatNum(..), fromInt, fromStr, toInt, toStr)

{-| Implements a natural number type. Feel free to extend with functions for addition, subtraction etc.
-}


type NatNum
    = NatNum Int


fromInt : Int -> NatNum
fromInt int =
    NatNum <|
        if int < 0 then
            0

        else
            int


fromStr : String -> Maybe NatNum
fromStr str =
    Maybe.map fromInt (String.toInt str)


toStr : NatNum -> String
toStr (NatNum num) =
    String.fromInt num


toInt : NatNum -> Int
toInt val =
    case val of
        NatNum int ->
            int
