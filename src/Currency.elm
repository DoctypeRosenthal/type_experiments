module Currency exposing (Amount(..), Currency(..), amount, amountFromStr, amountToStr, currencyFromCountry, currencySymbol, euro, getCurrency, symbolFromAmount, usd)


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


currencyFromCountry : String -> Maybe Currency
currencyFromCountry s =
    if List.member (String.toLower s) [ "de", "es", "fr", "it" ] then
        Just EUR

    else
        case String.toLower s of
            "us" ->
                Just USD

            _ ->
                Nothing
