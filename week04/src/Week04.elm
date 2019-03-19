module Week04 exposing
    (  Country
    , Mottos
    , User
    , decodeDate
    , decodeMottos
    , decodeUser
    , mottos
    )

import Dict exposing (Dict)
import Iso8601
import Json.Decode as J
import Parser
import Time


type alias User =
    { name : String
    , cats : Maybe Int
    }


decodeUser : J.Decoder User
decodeUser =
    J.map2 User
        (J.field "name" J.string)
        (J.field "cats" (J.maybe J.int))


mottos : String
mottos =
    """
{"Germany": {"motto": "Einigkeit und Recht und Freiheit", "currency": "EUR"},
 "England": {"motto": "God Save the Queen", "currency": "GBP"},
 "France": {"motto": "Liberté, Égalité, Fraternité", "currency": "EUR"}}
"""


type alias Mottos =
    { countries : List Country }


type alias Country =
    { name : String
    , motto : String
    , currency : String
    }


type alias CountryInfo =
    { motto : String
    , currency : String
    }


decodeMottos : J.Decoder Mottos
decodeMottos =
    let
        decodeCountryInfo : J.Decoder CountryInfo
        decodeCountryInfo =
            J.map2 CountryInfo
                (J.field "motto" J.string)
                (J.field "currency" J.string)

        mkCountry : String -> CountryInfo -> Country
        mkCountry name info =
            Country name info.motto info.currency

        countryInfosToMottos : Dict String CountryInfo -> Mottos
        countryInfosToMottos =
            Mottos << Dict.values << Dict.map mkCountry
    in
    J.map countryInfosToMottos (J.dict decodeCountryInfo)


decodeDate : J.Decoder Time.Posix
decodeDate =
    let
{--
        Parser.deadEndsToString : List DeadEnd -> String
        Iso8601.toTime : String -> Result (List DeadEnd) Posix
        J.string : J.Decoder String
--}
        parseTime : String -> J.Decoder Time.Posix
        parseTime str =
            case Iso8601.toTime str of
                Err deadEnds ->
                    J.fail (Parser.deadEndsToString deadEnds)

                Ok posix ->
                    J.succeed posix
    in
    J.string |> J.andThen parseTime
