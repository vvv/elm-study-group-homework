module Week04 exposing (Country, CountryInfo, Mottos, User, decodeMottos, decodeUser, mottos)

--XXX (mottos, decodeMottos, decodeUser)

import Dict exposing (Dict)
import Json.Decode as J


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
