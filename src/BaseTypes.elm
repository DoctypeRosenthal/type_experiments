module BaseTypes exposing (NatNum, Percent, natNum, natNumFromStr, natNumToInt, natNumToStr, percent, percentFromStr, percentToInt, percentToStr)

{-| Please mind that all types in this module should be opaque and only their constructor functions should be exposed
-}


type NatNum
    = NatNum Int


type Percent
    = Percent Int


natNum : Int -> NatNum
natNum int =
    NatNum <|
        if int < 0 then
            0

        else
            int


natNumFromStr : String -> Maybe NatNum
natNumFromStr str =
    Maybe.map natNum (String.toInt str)


natNumToStr : NatNum -> String
natNumToStr (NatNum num) =
    String.fromInt num


natNumToInt : NatNum -> Int
natNumToInt val =
    case val of
        NatNum int ->
            int


percent : Int -> Percent
percent =
    Percent << clamp 0 100


percentFromStr : String -> Maybe Percent
percentFromStr str =
    Maybe.map percent (String.toInt str)


percentToStr : Percent -> String
percentToStr (Percent num) =
    String.fromInt num


percentToInt : Percent -> Int
percentToInt val =
    case val of
        Percent int ->
            int
