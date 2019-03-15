module BaseTypes exposing (Amount, Currency, NatNum, Percent, amount, amountFromStr, amountToStr, currencySymbol, euro, getCurrency, natNum, natNumFromStr, natNumToInt, natNumToStr, percent, percentFromStr, percentToInt, percentToStr, usd, symbolFromAmount)

{-| Please mind that all types in this module should be opaque and only their constructor functions should be exposed
-}


type NatNum
    = NatNum Int


type Percent
    = Percent Int


type Currency
    = EUR
    | USD


type Amount
    = Amount Currency Float


amount : Currency -> Float -> Amount
amount currency float =
    Amount currency float


amountFromStr : Currency -> String -> Maybe Amount
amountFromStr currency string =
    Maybe.map (amount currency) (String.toFloat string)


euro : Currency
euro =
    EUR


usd : Currency
usd =
    USD


amountToStr : Amount -> String
amountToStr (Amount _ float) =
    String.fromFloat float


getCurrency : Amount -> Currency
getCurrency (Amount currency _) =
    currency


currencySymbol : Currency -> String
currencySymbol currency =
    case currency of
        EUR ->
            "€"

        USD ->
            "$"


symbolFromAmount : Amount -> String
symbolFromAmount =
    getCurrency >> currencySymbol


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
