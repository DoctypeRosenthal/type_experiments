module BaseTypes.Percent exposing (Percent(..), fromInt, fromStr, toInt, toStr)

{-| Implements a percent. Feel free to extend with functions for addition, subtraction etc.
-}


type Percent
    = Percent Int


fromInt : Int -> Percent
fromInt =
    Percent << clamp 0 100


fromStr : String -> Maybe Percent
fromStr str =
    Maybe.map fromInt (String.toInt str)


toStr : Percent -> String
toStr (Percent num) =
    String.fromInt num


toInt : Percent -> Int
toInt val =
    case val of
        Percent int ->
            int
